module Database.Orville.Internal.TableDefinition where

import qualified  Data.List as List

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.FieldUpdate
import            Database.Orville.Internal.Types

tableColumnNames :: TableDefinition entity -> [String]
tableColumnNames = map fieldName . tableFields

tablePrimaryKey :: TableDefinition entity -> FieldDefinition
tablePrimaryKey tableDef =
  case List.find isPrimaryKeyField (tableFields tableDef) of
    Just field -> field
    Nothing -> error $ "No primary key defined for " ++ tableName tableDef

mkInsertClause :: TableDefinition entity -> String
mkInsertClause tableDef =
    "INSERT INTO \"" ++ tableName tableDef ++
    "\" (" ++ columns ++ ") VALUES (" ++ placeholders ++ ")"

  where insertFields = filter (not . isUninsertedField)
                              (tableFields tableDef)

        columns = List.intercalate "," $ map fieldName insertFields
        placeholders = List.intercalate "," $ map (const "?") insertFields

mkUpdateClause :: TableDefinition entity -> [FieldUpdate] -> String
mkUpdateClause tableDef updates =
    "UPDATE \"" ++ tableName tableDef ++
    "\" SET " ++ placeholders
  where placeholders = List.intercalate "," $ map fieldUpdateSql updates

mkSelectClause :: TableDefinition entity -> String
mkSelectClause tableDef =
    "SELECT " ++ columns ++ " FROM \"" ++ tableName tableDef ++ "\""
  where columns = List.intercalate ", " $ tableColumnNames tableDef

mkDeleteClause :: TableDefinition entity -> String
mkDeleteClause tableDef =
    "DELETE FROM \"" ++ tableName tableDef ++ "\""

