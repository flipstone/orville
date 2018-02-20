{-|
Module    : Database.Orville.Popper
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Popper
  ( PopError(..)
  , Popper
  , Popped(..)
  , (>>>), (<<<)
  , abortPop
  , certainly
  , certainly'
  , fromKern
  , hasMany
  , hasManyIn
  , hasOneIn
  , hasManyInWhere
  , hasManyWhere
  , hasOne
  , hasOne'
  , hasOneWhere
  , kern
  , missingRecordMessage
  , popMissingRecord
  , onKern
  , pop, popThrow
  , popFirst
  , popMany
  , onPopMany
  , popMaybe
  , popQuery
  , popRecord
  , popRecord'
  , popTable
  ) where

import            Prelude hiding ((.))

import            Control.Arrow
import            Control.Applicative
import            Control.Category
import            Control.Monad.Catch
import            Data.Convertible
import            Data.Either
import qualified  Data.Map.Strict as Map
import            Data.Maybe

import            Database.Orville.Core
import            Database.Orville.Internal.QueryCache

-- Simple helpers and such that make the public API

kern :: Popper a a
kern = PopId

fromKern :: (a -> b) -> Popper a b
fromKern f = f <$> kern

onKern :: (a -> b -> c) -> Popper b a -> Popper b c
onKern mkPuff popper = mkPuff <$> popper <*> kern

liftPop :: (a -> Popped b) -> Popper a b
liftPop = PopLift

abortPop :: PopError -> Popper a b
abortPop = PopAbort

popQuery :: Orville b -> Popper a b
popQuery = PopQuery

certainly :: PopError-> Popper (Maybe b) b
certainly err = liftPop $ maybe (PoppedError err) PoppedValue

certainly' :: Popper a PopError
           -> Popper a (Maybe b)
           -> Popper a b
certainly' msgPopper bPopper =
  (msgPopper &&& bPopper) >>>
  liftPop (\(err, maybeB) -> maybe (PoppedError err) PoppedValue maybeB)

popRecord :: TableDefinition entity
          -> Record
          -> Popper a (Maybe (entity Record))
popRecord tableDef recordId = popQuery (findRecord tableDef recordId)

popRecord' :: TableDefinition entity
           -> Record
           -> Popper a (entity Record)
popRecord' td ri =
    popRecord td ri >>> certainly err
  where
    err = MissingRecord td (tablePrimaryKey td) (convert ri)

popFirst :: TableDefinition entity
         -> SelectOptions
         -> Popper a (Maybe (entity Record))
popFirst tableDef opts = popQuery (selectFirst tableDef opts)

popTable :: TableDefinition entity
         -> SelectOptions
         -> Popper a ([entity Record])
popTable tableDef opts = popQuery (selectAll tableDef opts)

popMaybe :: Popper a b -> Popper (Maybe a) (Maybe b)
popMaybe = PopMaybe

hasMany :: TableDefinition entity
        -> FieldDefinition
        -> Popper Record [entity Record]
hasMany tableDef fieldDef = PopRecordManyBy tableDef fieldDef mempty

hasOneIn :: TableDefinition entity
        -> FieldDefinition
        -> Popper [Record] (Map.Map Record (entity Record))
hasOneIn tableDef fieldDef = PopRecordsBy tableDef fieldDef mempty

hasManyIn :: TableDefinition entity
        -> FieldDefinition
        -> Popper [Record] (Map.Map Record [entity Record])
hasManyIn tableDef fieldDef = PopRecordsManyBy tableDef fieldDef mempty

hasManyWhere :: TableDefinition entity
             -> FieldDefinition
             -> SelectOptions
             -> Popper Record [entity Record]
hasManyWhere = PopRecordManyBy

hasManyInWhere :: TableDefinition entity
               -> FieldDefinition
               -> SelectOptions
               -> Popper [Record] (Map.Map Record [entity Record])
hasManyInWhere = PopRecordsManyBy

hasOne :: ( Convertible a SqlValue
          , Convertible SqlValue a
          , Ord a
          )
       => TableDefinition entity
       -> FieldDefinition
       -> Popper a (Maybe (entity Record))
hasOne tableDef fieldDef =
  hasOneWhere tableDef fieldDef mempty

hasOneWhere :: ( Convertible a SqlValue
               , Convertible SqlValue a
               , Ord a
               )
            => TableDefinition entity
            -> FieldDefinition
            -> SelectOptions
            -> Popper a (Maybe (entity Record))
hasOneWhere = PopRecordBy

hasOne' :: ( Convertible a SqlValue
           , Convertible SqlValue a
           , Ord a
           )
        => TableDefinition entity
        -> FieldDefinition
        -> Popper a (entity Record)
hasOne' tableDef fieldDef =
  certainly' (popMissingRecord tableDef fieldDef) (hasOne tableDef fieldDef)

popMissingRecord :: Convertible a SqlValue
                 => TableDefinition entity
                 -> FieldDefinition
                 -> Popper a PopError
popMissingRecord tableDef fieldDef =
  fromKern (MissingRecord tableDef fieldDef . convert)


-- popMany is the most involved helper. It recursively
-- rewrites the entire Popper expression to avoid
-- running the popper's queries individually for each
-- item in the list.
--
-- This is where the magic happens.
--

popMany :: Popper a b -> Popper [a] [b]
popMany (PopOnMany _ manyPopper) = manyPopper

popMany (PopRecord tableDef) =
  zipWith Map.lookup <$> kern
                     <*> (repeat <$> PopRecords tableDef)

popMany (PopRecordBy tableDef fieldDef selectOptions) =
  zipWith Map.lookup <$> kern
                     <*> (repeat <$> PopRecordsBy tableDef fieldDef selectOptions)

popMany (PopRecordManyBy tableDef fieldDef opts) =
    zipWith getRecords <$> kern
                       <*> (repeat <$> PopRecordsManyBy tableDef fieldDef opts)
  where

    getRecords key = fromMaybe [] . Map.lookup key

popMany (PopRecords tableDef) =
    (zipWith restrictMap)
      <$> kern
      <*> (fromKern concat >>>
           PopRecords tableDef >>>
           fromKern repeat)

  where restrictMap keys = Map.filterWithKey (isKey keys)
        isKey keys key _ = key `elem` keys

popMany (PopRecordsManyBy tableDef fieldDef opts) =
    (zipWith restrictMap)
      <$> kern
      <*> (fromKern concat >>>
           PopRecordsManyBy tableDef fieldDef opts >>>
           fromKern repeat)

  where restrictMap keys = Map.filterWithKey (isKey keys)
        isKey keys key _ = key `elem` keys

popMany (PopRecordsBy tableDef fieldDef opts) =
    (zipWith restrictMap)
      <$> kern
      <*> (fromKern concat >>>
           PopRecordsBy tableDef fieldDef opts >>>
           fromKern repeat)

  where restrictMap keys = Map.filterWithKey (isKey keys)
        isKey keys key _ = key `elem` keys

popMany PopId = PopId
popMany (PopAbort err) = (PopAbort err)
popMany (PopPure a) =
  PopPure (repeat a) -- lists here should be treated as
                     -- ZipLists, so 'repeat' is 'pure'

popMany (PopLift f) = PopLift $ \inputs ->
  let poppeds = map f inputs

      extract (PoppedValue b) (PoppedValue bs) = PoppedValue (b : bs)
      extract _ (PoppedError err) = PoppedError err
      extract (PoppedError err) _ = PoppedError err

  in foldr extract (PoppedValue []) poppeds

popMany (PopMap f popper) = PopMap (fmap f) (popMany popper)
popMany (PopApply fPopper aPopper) =
  getZipList <$>
    PopApply (fmap (<*>) (ZipList <$> popMany fPopper))
             (ZipList <$> popMany aPopper)

popMany (PopChain bPopper aPopper) =
  PopChain (popMany bPopper) (popMany aPopper)

popMany (PopArrowFirst popper) =
  fromKern unzip >>>
  first (popMany popper) >>>
  fromKern (uncurry zip)

popMany (PopArrowLeft popper) =
    rebuildList <$> kern
                <*> (fromKern lefts >>> popMany popper)
  where
    rebuildList [] _ = []
    rebuildList (Left _ : _) [] = [] -- shouldn't happen?

    rebuildList (Right c : eacs) bs = Right c : rebuildList eacs bs
    rebuildList (Left _ : eacs) (b : bs) = Left b : rebuildList eacs bs

popMany (PopMaybe singlePopper) =
    rebuildList <$> kern
                <*> (fromKern catMaybes >>> popMany singlePopper)
  where
    rebuildList [] _ = []
    rebuildList (Just _ : _) [] = [] -- shouldn't happen?

    rebuildList (Nothing : as) bs = Nothing : rebuildList as bs
    rebuildList (Just _ : as) (b : bs) = Just b : rebuildList as bs

popMany (PopQuery orm) =
  -- the orm query here doesn't depend on the Popper input,
  -- so we can run it once and then construct an infinite
  -- list of the results to be lined up as the outputs for
  -- each input in the list
  --
  -- ('repeat' is 'pure' since lists here are zipped)
  --
  PopQuery (repeat <$> orm)

-- Manually specifies the many handling of a popper. The original popper
-- is lift unchanged. If it becomes involved in a `popMany` operation, the
-- provided popper with be substituted for it, rather than apply the normal
-- popper re-write rules.
--
-- This is useful if either of the cases can manually optimized in a way that
-- is incompatible with the normal popping mechanisms
--
onPopMany :: Popper a b -> Popper [a] [b] -> Popper a b
onPopMany = PopOnMany

-- The Popper guts

data PopError =
    forall ent. MissingRecord (TableDefinition ent) FieldDefinition SqlValue
  | Unpoppable String

instance Show PopError where
  show (MissingRecord tableDef fieldDef fieldValue) =
    "MissingRecord: " ++ missingRecordMessage tableDef fieldDef fieldValue

  show (Unpoppable msg) =
    "Unpoppable: " ++ msg

missingRecordMessage :: TableDefinition entity
                     -> FieldDefinition
                     -> SqlValue
                     -> String
missingRecordMessage tableDef fieldDef fieldValue =
  concat [ "Unable to find "
         , tableName tableDef
         , " with "
         , fieldName fieldDef
         , " = "
         , show fieldValue
         ]

instance Exception PopError

data Popped a =
    PoppedValue a
  | PoppedError PopError

instance Functor Popped where
  fmap f (PoppedValue a) = PoppedValue (f a)
  fmap _ (PoppedError err) = PoppedError err

instance Applicative Popped where
  pure = PoppedValue
  (PoppedValue f) <*> (PoppedValue a) = PoppedValue (f a)
  (PoppedError err) <*> _ = PoppedError err
  _ <*> (PoppedError err) = PoppedError err


-- Popper GADT. This defines the popper expression dsl
-- that is used internally to represent poppers. These
-- constructors are not exposed, so they can be changed
-- freely as long as the exported API is stable.
--
data Popper a b where
  PopQuery :: Orville b -> Popper a b

  PopRecord :: TableDefinition entity
            -> Popper Record (Maybe (entity Record))

  PopRecords :: TableDefinition entity
             -> Popper [Record] (Map.Map Record (entity Record))

  PopRecordBy :: ( Convertible c SqlValue
                 , Convertible SqlValue c
                 , Ord c
                 )
              => TableDefinition entity
              -> FieldDefinition
              -> SelectOptions
              -> Popper c (Maybe (entity Record))

  PopRecordManyBy :: TableDefinition entity
                  -> FieldDefinition
                  -> SelectOptions
                  -> Popper Record [entity Record]

  PopRecordsBy :: ( Convertible c SqlValue
                  , Convertible SqlValue c
                  , Ord c
                  )
               => TableDefinition entity
               -> FieldDefinition
               -> SelectOptions
               -> Popper [c] (Map.Map c (entity Record))

  PopRecordsManyBy :: TableDefinition entity
                   -> FieldDefinition
                   -> SelectOptions
                   -> Popper [Record] (Map.Map Record [entity Record])

  PopId :: Popper a a
  PopPure :: b -> Popper a b
  PopLift :: (a -> Popped b) -> Popper a b
  PopAbort :: PopError -> Popper a b

  PopMap :: (b -> c) -> Popper a b -> Popper a c
  PopApply :: Popper a (b -> c)
           -> Popper a b
           -> Popper a c

  PopChain :: Popper b c -> Popper a b -> Popper a c
  PopArrowFirst :: Popper a b -> Popper (a, c) (b, c)
  PopArrowLeft :: Popper a b -> Popper (Either a c) (Either b c)

  PopOnMany :: Popper a b -> Popper [a] [b] -> Popper a b

  PopMaybe :: Popper a b -> Popper (Maybe a) (Maybe b)

instance Functor (Popper a) where
  fmap = PopMap

instance Applicative (Popper a) where
  pure = PopPure
  (<*>) = PopApply

instance Category Popper where
  id = PopId
  (.) = PopChain

instance Arrow Popper where
  arr = fromKern
  first = PopArrowFirst

instance ArrowChoice Popper where
  left = PopArrowLeft

popThrow :: Popper a b -> a -> Orville b
popThrow popper a = do
  popped <- pop popper a

  case popped of
    PoppedValue b -> return b
    PoppedError e -> throwM e

-- This is where the action happens. pop converts the
-- Popper DSL into Orville calls with the provided input
--
pop :: Popper a b -> a -> Orville (Popped b)
pop popper a = runQueryCached $ popCached popper a

popCached :: (MonadThrow m, MonadOrville conn m)
          => Popper a b -> a -> QueryCached m (Popped b)
popCached (PopOnMany singlePopper _) a =
  popCached singlePopper a

popCached (PopQuery query) _ =
  PoppedValue <$> unsafeLift query

popCached (PopRecordBy tableDef fieldDef opts) recordId =
  PoppedValue <$> selectFirstCached tableDef
                                    (  where_ (fieldDef .== recordId)
                                    <> opts
                                    )

popCached (PopRecordManyBy tableDef fieldDef opts) recordId =
  PoppedValue <$> selectCached tableDef
                               (  where_ (fieldDef .== recordId)
                               <> opts
                               )

popCached (PopRecordsBy tableDef fieldDef opts) recordIds =
  PoppedValue <$> Map.map head
              <$> findRecordsByCached tableDef
                                      fieldDef
                                      (  where_ (fieldDef .<- recordIds)
                                      <> opts
                                      )

popCached (PopRecordsManyBy tableDef fieldDef opts) recordIds =
  PoppedValue <$> findRecordsByCached tableDef
                                      fieldDef
                                      (  where_ (fieldDef .<- recordIds)
                                      <> opts
                                      )

popCached (PopRecord tableDef) recordId =
  PoppedValue <$> findRecordCached tableDef recordId

popCached (PopRecords tableDef) recordIds =
  PoppedValue <$> findRecordsCached tableDef recordIds

popCached (PopAbort err) _ =
  pure (PoppedError err)

popCached PopId a =
  pure (PoppedValue a)

popCached (PopPure a) _ =
  pure (PoppedValue a)

popCached (PopLift f) a =
  pure (f a)

popCached (PopMap f popper) a =
  fmap f <$> popCached popper a

popCached (PopApply fPopper bPopper) a =
  (fmap (<*>) (popCached fPopper a)) <*> popCached bPopper a

popCached (PopChain popperB popperA) a = do
  value <- popCached popperA a

  case value of
    PoppedError err -> pure (PoppedError err)
    PoppedValue b -> popCached popperB b

popCached (PopArrowFirst popper) (a,c) = do
  poppedB <- popCached popper a

  case poppedB of
    PoppedValue b -> return (PoppedValue (b, c))
    PoppedError err -> return (PoppedError err)

popCached (PopArrowLeft popper) ac = do
  case ac of
    Left a -> fmap Left <$> popCached popper a
    Right c -> pure (PoppedValue (Right c))

popCached (PopMaybe popper) a =
  case a of
    Nothing -> pure (PoppedValue Nothing)
    Just val -> fmap Just <$> popCached popper val
