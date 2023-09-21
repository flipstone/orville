{-|
Module    : Database.Orville.PostgreSQL.Internal.IndexDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.IndexDefinition
  ( uniqueIndex
  , simpleIndex
  , simplePartialIndex
  , uniquePartialIndex
  ) where

import Data.List (intercalate)

import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.Types

{- |
  Migration Guide: @uniqueIndex@ no longer requires a name to be specified.
  Migration will be done automatically by inspecting the structure of the
  indexes that exist in the database. It also no longer accepts a
  @TableDefinition@ at the time of creating the @IndexDefinition@. Instead you
  should use @addTableIndexes@ to add the @IndexDefinition@ to the
  @TableDefinition@ for the table that you wish to index.

  If you wish to specify the index name explicitly, you can use
  @uniqueNamedIndex@ instead. If you do so, index migration will be managed by
  comparing to the names of existing indexes rather than checking that the
  index structure matches the Haskell definition.
-}
uniqueIndex ::
     String
  -> TableDefinition readEntity writeEntity key
  -> [SomeField]
  -> IndexDefinition
uniqueIndex name tableDef fields =
  mkIndexDefinition True name tableDef fields []

{- |
  Migration Guide: @simpleIndex@ has been renamed to @nonUniqueIndex@. It no
  longer requires a name to be specified. Migration will be done automatically
  be inspecting the structure of the indexes that exist in the database. It
  also no longer accepts a @TableDefinition@ at the time of creating the
  @IndexDefinition@. Instead you should use @addTableIndexes@ to add the
  @IndexDefinition@ to the @TableDefinition@ for the table that you wish to
  index.

  If you wish to specify the index name explicitly, you can use
  @nonUniqueNamedIndex@ instead. If you do so, index migration will be managed
  by comparing to the names of existing indexes rather than checking that the
  index structure matches the Haskell definition.
-}
simpleIndex ::
     String
  -> TableDefinition readEntity writeEntity key
  -> [SomeField]
  -> IndexDefinition
simpleIndex name tableDef fields =
  mkIndexDefinition False name tableDef fields []

indexFieldsBody :: [SomeField] -> String
indexFieldsBody fields = "(" ++ intercalate "," (map name fields) ++ ")"
  where
    name (SomeField field) = escapedFieldName field

-- | Works much the same as `uniqueIndex` but takes a list of strings that are the conditions of a
-- where clause on index creation for partial indexes
uniquePartialIndex :: String
                   -> TableDefinition readEntity writeEntity key
                   -> [SomeField]
                   -> [String]
                   -> IndexDefinition
uniquePartialIndex =
  mkIndexDefinition True

-- | Works much the same as `simpleIndex` but takes a list of strings that are the conditions of a
-- where clause on index creation for partial indexes
simplePartialIndex :: String
                   -> TableDefinition readEntity writeEntity key
                   -> [SomeField]
                   -> [String]
                   -> IndexDefinition
simplePartialIndex =
  mkIndexDefinition False

mkIndexDefinition :: Bool
                  -> String
                  -> TableDefinition readEntity writeEntity key
                  -> [SomeField]
                  -> [String]
                  -> IndexDefinition
mkIndexDefinition unique name tableDef fields whereStrs =
  let
    whereStr [] = ""
    whereStr strs = "WHERE " <> intercalate " AND " (fmap (\a -> "(" <> a <> ")") strs)
  in
    IndexDefinition
      { indexName = name
      , indexUnique = unique
      , indexTable = tableName tableDef
      , indexBody = indexFieldsBody fields <> whereStr whereStrs
      }
