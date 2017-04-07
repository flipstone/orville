module Database.Orville.Internal.TableDefinition where

import qualified  Data.List as List

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

tableColumnNames :: TableDefinition entity -> [String]
tableColumnNames = map fieldName . tableFields

insertableColumnNames :: TableDefinition entity -> [String]
insertableColumnNames = map fieldName . filter (not . isUninsertedField) . tableFields

tablePrimaryKey :: TableDefinition entity -> FieldDefinition
tablePrimaryKey tableDef =
  case List.find isPrimaryKeyField (tableFields tableDef) of
    Just field -> field
    Nothing -> error $ "No primary key defined for " ++ tableName tableDef

