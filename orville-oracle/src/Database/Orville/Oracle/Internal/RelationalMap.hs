{-|
Module    : Database.Orville.Oracle.Internal.RelationalMap
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Oracle.Internal.RelationalMap
  ( mkTableDefinition
  , TableParams(..)
  , RelationalMap
  , mapAttr
  , mapField
  , attrField
  , maybeMapper
  , prefixMap
  , partialMap
  , readOnlyMap
  , readOnlyField
  ) where

import Control.Monad (join, when)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Data.Profunctor (Profunctor(lmap, rmap))

import Database.Orville.Oracle.Internal.FieldDefinition
import Database.Orville.Oracle.Internal.FromSql
import Database.Orville.Oracle.Internal.Types

{-|
 'TableParams' is the simplest way to make a 'TableDefinition'. You
 can use 'mkTableDefinition' to make a definition from the simplified
 params. Where 'TableDefinition' requires the 'tableFields', 'tableFromSql',
 and 'tableToSql' to all be defined separately and kept in sync, 'TableParams'
 provides a single 'tblMapper' field that specifies all three simultaneously
 and ensures they are consistent with one another.
 -}
data TableParams readEntity writeEntity key = TableParams
  { tblName :: String
      -- ^ The name of the table in the database
  , tblMapper :: RelationalMap writeEntity readEntity
      -- ^ The relational mapping that defines how the Haskell entity type
      -- is converted both to and from sql. The fields utilized in the mapping
      -- are used to automatically build the list of 'FieldDefinitions' that
      -- define the structure of the table in the database.
  , tblSafeToDelete :: [String]
      -- ^ A list of any columns that may be deleted from the table by Orville.
      -- (Orville will never delete a column without being told it is safe)
  , tblPrimaryKey :: FieldDefinition key
      -- ^ A function to set the key on the entity
  , tblGetKey :: readEntity -> key
      -- ^ A function to get the key on the entity
  , tblComments :: TableComments ()
      -- ^ Any comments that might be interesting for developers to see. These
      -- comments will get printed in the log if there is an erro while attempting
      -- to migrate the table.
  }

{-|
 'mkTableDefinition' converts a 'TableParams' to 'TableDefinition'. Usually
 this is used directly on a record literal of the 'TableParams'. For
 example:

 @
  data Foo key = Foo key { fooId :: Record }
  myTable :: TableDefinition Foo
  myTable = mkTableDefinition $
    TableParams
      { tblName = "foo"
      , tblMapper = User <$> attrField fooId idField
      , tableSafeToDelete = []
      , tblSetKey = \key foo -> foo { fooId = key }
      , tblGetKey = fooId
      , tblComments = []
      }

 @
 -}
mkTableDefinition ::
     TableParams readEntity writeEntity key
  -> TableDefinition readEntity writeEntity key
mkTableDefinition (TableParams {..}) =
  TableDefinition
    { tableFields = fields tblMapper
    , tableFromSql = mkFromSql tblMapper
    , tableToSql = mkToSql tblMapper
    , tablePrimaryKey = tblPrimaryKey
    , tableName = tblName
    , tableSafeToDelete = tblSafeToDelete
    , tableGetKey = tblGetKey
    , tableComments = tblComments
    }

data RelationalMap a b where
  RM_Field :: FieldDefinition a -> RelationalMap a a
  RM_Nest :: (a -> b) -> RelationalMap b c -> RelationalMap a c
  RM_Pure :: b -> RelationalMap a b
  RM_Apply
    :: RelationalMap a (b -> c) -> RelationalMap a b -> RelationalMap a c
  RM_Partial :: RelationalMap a (Either String a) -> RelationalMap a a
  RM_ReadOnly :: RelationalMap a b -> RelationalMap c b
  RM_MaybeTag
    :: RelationalMap (Maybe a) (Maybe b) -> RelationalMap (Maybe a) (Maybe b)

instance Functor (RelationalMap a) where
  fmap f rm = pure f <*> rm

instance Applicative (RelationalMap a) where
  pure = RM_Pure
  (<*>) = RM_Apply

instance Profunctor RelationalMap where
  rmap = fmap
  lmap = mapAttr

mapAttr :: (a -> b) -> RelationalMap b c -> RelationalMap a c
mapAttr = RM_Nest

mapField :: FieldDefinition a -> RelationalMap a a
mapField = RM_Field

partialMap :: RelationalMap a (Either String a) -> RelationalMap a a
partialMap = RM_Partial

readOnlyMap :: RelationalMap a b -> RelationalMap c b
readOnlyMap = RM_ReadOnly

attrField :: (a -> b) -> FieldDefinition b -> RelationalMap a b
attrField get = mapAttr get . mapField

readOnlyField :: FieldDefinition a -> RelationalMap b a
readOnlyField = readOnlyMap . mapField

prefixMap :: String -> RelationalMap a b -> RelationalMap a b
prefixMap prefix (RM_Nest f rm) = RM_Nest f (prefixMap prefix rm)
prefixMap prefix (RM_Field f) = RM_Field (f `withPrefix` prefix)
prefixMap prefix (RM_Apply rmF rmA) =
  RM_Apply (prefixMap prefix rmF) (prefixMap prefix rmA)
prefixMap prefix (RM_Partial rm) = RM_Partial (prefixMap prefix rm)
prefixMap prefix (RM_ReadOnly rm) = RM_ReadOnly (prefixMap prefix rm)
prefixMap prefix (RM_MaybeTag rm) = RM_MaybeTag (prefixMap prefix rm)
prefixMap _ rm@(RM_Pure _) = rm

maybeMapper :: RelationalMap a b -> RelationalMap (Maybe a) (Maybe b)
maybeMapper
    -- rewrite the mapper to handle null fields, then tag
    -- it as having been done so we don't double-map it
    -- in a future `maybeMapper` call.
    --
 = RM_MaybeTag . go
  where
    go :: RelationalMap a b -> RelationalMap (Maybe a) (Maybe b)
    go (RM_Nest f rm) = RM_Nest (fmap f) (go rm)
    go (RM_Field f) = RM_Field (nullableField f)
    go (RM_Pure a) = RM_Pure (pure a)
    go (RM_Apply rmF rmA) = RM_Apply (fmap (<*>) $ go rmF) (go rmA)
    go (RM_Partial rm) = RM_Partial (flipError <$> go rm)
      where
        flipError :: Maybe (Either String a) -> Either String (Maybe a)
        flipError (Just (Right a)) = Right (Just a)
        flipError (Just (Left err)) = Left err
        flipError Nothing = Right Nothing
    go (RM_ReadOnly rm) = RM_ReadOnly (go rm)
    go rm@(RM_MaybeTag _) = fmap Just $ mapAttr join $ rm

fields :: RelationalMap a b -> [SomeField]
fields (RM_Field field) = [SomeField field]
fields (RM_Apply rm1 rm2) = fields rm1 ++ fields rm2
fields (RM_Nest _ rm) = fields rm
fields (RM_Partial rm) = fields rm
fields (RM_MaybeTag rm) = fields rm
fields (RM_Pure _) = []
fields (RM_ReadOnly rm) =
  map (someFieldWithFlag AssignedByDatabase) (fields rm)
  where
    someFieldWithFlag flag (SomeField f) = SomeField (f `withFlag` flag)

mkFromSql :: RelationalMap a b -> FromSql b
mkFromSql (RM_Field field) = fieldFromSql field
mkFromSql (RM_Nest _ rm) = mkFromSql rm
mkFromSql (RM_ReadOnly rm) = mkFromSql rm
mkFromSql (RM_MaybeTag rm) = mkFromSql rm
mkFromSql (RM_Pure b) = pure b
mkFromSql (RM_Apply rmF rmC) = mkFromSql rmF <*> mkFromSql rmC
mkFromSql (RM_Partial rm) = do
  joinFromSqlError (wrapError <$> mkFromSql rm)
  where
    wrapError = either (Left . RowDataError) Right

mkToSql :: RelationalMap a b -> ToSql a ()
mkToSql (RM_Field field) =
  when (not $ isAssignedByDatabaseField field) $ do
    value <- ask
    modify (fieldToSqlValue field value :)
mkToSql (RM_Nest f rm) = getComponent f (mkToSql rm)
mkToSql (RM_Apply rmF rmC) = mkToSql rmF >> mkToSql rmC
mkToSql (RM_Partial rm) = mkToSql rm
mkToSql (RM_MaybeTag rm) = mkToSql rm
mkToSql (RM_ReadOnly _) = pure ()
mkToSql (RM_Pure _) = pure ()
