{-|
Module    : Database.Orville.Internal.IndexDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.Internal.IndexDefinition
  ( uniqueIndex, simpleIndex
  , uniquePartialIndex, partialIndex
  ) where

import            Data.List (intercalate)

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types
import            Database.Orville.Internal.Where

uniquePartialIndex :: String -> TableDefinition entity -> [FieldDefinition] -> [WhereCondition] -> IndexDefinition
uniquePartialIndex name tableDef fields whereConditions =
  IndexDefinition {
    indexName = name
  , indexUnique = True
  , indexTable = tableName tableDef
  , indexBody = intercalate " " [ indexFieldsBody fields, whereClause whereConditions ]
  }

partialIndex :: String -> TableDefinition entity -> [FieldDefinition] -> [WhereCondition] -> IndexDefinition
partialIndex name tableDef fields whereConditions =
  IndexDefinition {
    indexName = name
  , indexUnique = True
  , indexTable = tableName tableDef
  , indexBody = intercalate " " [ indexFieldsBody fields, whereClause whereConditions ]
  }

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
