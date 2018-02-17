{-|
Module    : Database.Orville.Internal.IndexDefinition
Copyright : Fliptsone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.Internal.IndexDefinition
  ( uniqueIndex, simpleIndex
  ) where

import            Data.List (intercalate)

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

uniqueIndex :: String -> TableDefinition entity -> [FieldDefinition] -> IndexDefinition
uniqueIndex name tableDef fields =
  IndexDefinition {
    indexName = name
  , indexUnique = True
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

simpleIndex :: String -> TableDefinition entity -> [FieldDefinition] -> IndexDefinition
simpleIndex name tableDef fields =
  IndexDefinition {
    indexName = name
  , indexUnique = False
  , indexTable = tableName tableDef
  , indexBody = indexFieldsBody fields
  }

indexFieldsBody :: [FieldDefinition] -> String
indexFieldsBody fields = "(" ++ intercalate "," (map escapedFieldName fields) ++ ")"
