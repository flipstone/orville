{-|
Module    : Database.Orville.PostgreSQL.Internal.IndexDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.IndexDefinition
  ( uniqueIndex
  , simpleIndex
  ) where

import Data.List (intercalate)

import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.Types

uniqueIndex ::
     String
  -> TableDefinition readEntity writeEntity key
  -> [SomeField]
  -> IndexDefinition
uniqueIndex name tableDef fields =
  IndexDefinition
    { indexName = name
    , indexUnique = True
    , indexTable = tableName tableDef
    , indexBody = indexFieldsBody fields
    }

simpleIndex ::
     String
  -> TableDefinition readEntity writeEntity key
  -> [SomeField]
  -> IndexDefinition
simpleIndex name tableDef fields =
  IndexDefinition
    { indexName = name
    , indexUnique = False
    , indexTable = tableName tableDef
    , indexBody = indexFieldsBody fields
    }

indexFieldsBody :: [SomeField] -> String
indexFieldsBody fields = "(" ++ intercalate "," (map name fields) ++ ")"
  where
    name (SomeField field) = escapedFieldName field
