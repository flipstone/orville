{-|
Module    : Database.Orville.Internal.QueryCache
Copyright : Fliptsone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Internal.QueryCache
  ( QueryCached
  , runQueryCached
  , selectCached
  , selectFirstCached
  , findRecordCached
  , findRecordsCached
  , findRecordsByCached
  , unsafeLift
  )
  where

import            Control.Monad.Catch (MonadThrow)
import            Control.Monad.Trans
import            Control.Monad.Trans.State
import            Data.Convertible
import qualified  Data.Map as Map
import qualified  Data.Map.Helpers as Map
import            Data.Maybe
import            Data.Monoid
import            Data.String (fromString)
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Expr
import            Database.Orville.Internal.FromSql
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.QueryKey
import            Database.Orville.Internal.SelectOptions
import            Database.Orville.Internal.TableDefinition
import            Database.Orville.Internal.Types
import            Database.Orville.Internal.Where
import            Database.Orville.Select

type QueryCache = Map.Map QueryKey ResultSet

newtype QueryCached m a =
    QueryCached (StateT QueryCache m a)
  deriving
    (Functor, Applicative, Monad)

runQueryCached :: Monad m => QueryCached m a -> m a
runQueryCached (QueryCached statet) = evalStateT statet Map.empty

cached :: Monad m => QueryKey -> QueryCached m ResultSet -> QueryCached m ResultSet
cached key action = do
  cache <- QueryCached get

  case Map.lookup key cache of
    Just result -> do
      pure result
    Nothing -> do
      result <- action
      QueryCached $ put (Map.insert key result cache)
      pure result

selectCachedRows :: (MonadThrow m, MonadOrville conn m)
                 => TableDefinition entity
                 -> SelectOptions
                 -> QueryCached m ResultSet
selectCachedRows tableDef opts =
    cached key $ unsafeLift $
      runSelect $ selectQueryRows selects
                                  (fromClauseTable tableDef)
                                  opts
  where
    selects = expr . selectColumn . fromString <$> tableColumnNames tableDef
    key = mconcat [queryKey tableDef, queryKey opts]

selectCached :: (MonadThrow m, MonadOrville conn m)
             => TableDefinition entity
             -> SelectOptions
             -> QueryCached m [entity Record]
selectCached tableDef opts = do
  rows <- selectCachedRows tableDef opts
  unsafeLift $ decodeSqlRows (tableFromSql tableDef) rows

selectFirstCached :: (MonadThrow m, MonadOrville conn m)
                  => TableDefinition entity
                  -> SelectOptions
                  -> QueryCached m (Maybe (entity Record))
selectFirstCached tableDef opts =
  listToMaybe <$> selectCached tableDef (limit 1 <> opts)

findRecordsCached :: (MonadThrow m, MonadOrville conn m)
                  => TableDefinition entity
                  -> [Record]
                  -> QueryCached m (Map.Map Record (entity Record))
findRecordsCached tableDef recordIds = do
  let keyField = tablePrimaryKey tableDef
      mkEntry record = (tableGetKey tableDef record, record)

  recordList <- selectCached tableDef (where_ $ keyField .<- recordIds)
  pure $ Map.fromList (map mkEntry recordList)

findRecordCached :: (MonadThrow m, MonadOrville conn m)
                 => TableDefinition entity
                 -> Record
                 -> QueryCached m (Maybe (entity Record))
findRecordCached tableDef recordId = do
  let keyField = tablePrimaryKey tableDef
  selectFirstCached tableDef (where_ $ keyField .== recordId)

findRecordsByCached :: ( Convertible SqlValue fieldValue
                       , Ord fieldValue
                       , MonadThrow m
                       , MonadOrville conn m)
                    => TableDefinition entity
                    -> FieldDefinition
                    -> SelectOptions
                    -> QueryCached m (Map.Map fieldValue [entity Record])
findRecordsByCached tableDef field opts = do
  let builder = (,) <$> col field <*> tableFromSql tableDef
  rows <- selectCachedRows tableDef opts
  Map.groupBy' id <$> unsafeLift (decodeSqlRows builder rows)

-- this is unsafe in the sense that it does not provide
-- any guarantees that the action won't chance values in
-- the database, rendering the cache incorrect. It is not
-- exposed publically, but all usages of it here need to
-- be examined for correctness manually.
--
unsafeLift :: Monad m => m a -> QueryCached m a
unsafeLift = QueryCached . lift
