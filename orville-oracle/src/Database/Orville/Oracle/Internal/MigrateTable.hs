{-|
Module    : Database.Orville.Oracle.Internal.MigrateTable
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.Oracle.Internal.MigrateTable
  ( migrateTablePlan
  , dropTablePlan
  ) where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Database.HDBC

import Database.Orville.Oracle.Internal.MigrationPlan
import Database.Orville.Oracle.Internal.SchemaState
import Database.Orville.Oracle.Internal.SqlType
import Database.Orville.Oracle.Internal.Types

migrateTablePlan ::
     TableDefinition readEntity writeEntity key
  -> SchemaState
  -> Maybe MigrationPlan
migrateTablePlan tableDef schemaState =
  case schemaStateTableColumns (tableName tableDef) schemaState of
    Nothing ->
      Just $ migrationDDLForItem (Table tableDef) (mkCreateTableDDL tableDef)
    Just columns ->
      migrationDDLForItem (Table tableDef) <$>
      mkMigrateTableDDL columns tableDef

dropTablePlan :: String -> SchemaState -> Maybe MigrationPlan
dropTablePlan name schemaState = do
  guard (schemaStateTableExists name schemaState)
  pure $ migrationDDLForItem (DropTable name) (mkDropTableDDL name)

mkMigrateTableDDL ::
     [(String, SqlColDesc)]
  -> TableDefinition readEntity writeEntity key
  -> Maybe String
mkMigrateTableDDL columns tableDef =
  if null stmts
    then Nothing
    else Just $ "ALTER TABLE \"" ++ tableName tableDef ++ "\" " ++ cols
  where
    fields = tableFields tableDef
    fieldColumn fieldDef = lookup (fieldName fieldDef) columns
    colStmt (SomeField f) = mkMigrateColumnDDL f (fieldColumn f)
    dropStmt name = mkDropColumnDDL name (lookup name columns)
    stmts =
      List.concatMap colStmt fields ++
      List.concatMap dropStmt (tableSafeToDelete tableDef)
    cols = List.intercalate ", " $ stmts

mkMigrateColumnTypeDDL :: FieldDefinition a -> SqlColDesc -> Maybe String
mkMigrateColumnTypeDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
   in if colType fieldDesc /= colType colDesc ||
         colSize fieldDesc /= colSize colDesc
        then Just $
             "ALTER COLUMN " ++
             fieldName fieldDef ++
             " SET DATA TYPE " ++ sqlTypeDDL (fieldType fieldDef)
        else Nothing

mkMigrateColumnNullDDL :: FieldDefinition a -> SqlColDesc -> Maybe String
mkMigrateColumnNullDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
      fieldNull = fromMaybe True (colNullable fieldDesc)
      colNull = fromMaybe True (colNullable colDesc)
   in if fieldNull && not colNull
        then Just $ "ALTER COLUMN " ++ fieldName fieldDef ++ " DROP NOT NULL"
        else if not fieldNull && colNull
               then Just $
                    "ALTER COLUMN " ++ fieldName fieldDef ++ " SET NOT NULL"
               else Nothing

mkMigrateColumnDDL :: FieldDefinition a -> Maybe SqlColDesc -> [String]
mkMigrateColumnDDL fieldDef Nothing = ["ADD COLUMN " ++ mkFieldDDL fieldDef]
mkMigrateColumnDDL fieldDef (Just desc) =
  catMaybes
    [ mkMigrateColumnTypeDDL fieldDef desc
    , mkMigrateColumnNullDDL fieldDef desc
    ]

mkDropColumnDDL :: String -> Maybe SqlColDesc -> [String]
mkDropColumnDDL _ Nothing = []
mkDropColumnDDL name (Just _) = ["DROP COLUMN " ++ name]

mkFlagDDL :: ColumnFlag -> Maybe String
mkFlagDDL PrimaryKey = Just "PRIMARY KEY"
mkFlagDDL Unique = Just "UNIQUE"
mkFlagDDL (Default def) = Just $ "DEFAULT " ++ toColumnDefaultSql def
mkFlagDDL (References table field) =
  Just $ "REFERENCES \"" ++ tableName table ++ "\" (" ++ fieldName field ++ ")"
mkFlagDDL (ColumnDescription _) = Nothing
mkFlagDDL AssignedByDatabase = Nothing

mkFieldDDL :: FieldDefinition a -> String
mkFieldDDL field = name ++ " " ++ sqlType ++ " " ++ flagSql
  where
    name = fieldName field
    sqlType = sqlTypeDDL (fieldType field)
    flagSql =
      List.intercalate " " (notNull : mapMaybe mkFlagDDL (fieldFlags field))
    notNull =
      if sqlTypeNullable (fieldType field)
        then "NULL"
        else "NOT NULL"

mkCreateTableDDL :: TableDefinition readEntity writeEntity key -> String
mkCreateTableDDL tableDef =
  "CREATE TABLE \"" ++ tableName tableDef ++ "\" (" ++ fields ++ ")"
  where
    fields = List.intercalate ", " $ map mkSomeFieldDDL (tableFields tableDef)
    mkSomeFieldDDL (SomeField f) = mkFieldDDL f

mkDropTableDDL :: String -> String
mkDropTableDDL name = "DROP TABLE \"" ++ name ++ "\""

sqlFieldDesc :: FieldDefinition a -> SqlColDesc
sqlFieldDesc field =
  SqlColDesc
    { colType = sqlTypeId $ fieldType field
    , colSize = sqlTypeSqlSize $ fieldType field
    , colNullable = Just (sqlTypeNullable $ fieldType field)
    , colOctetLength = Nothing
    , colDecDigits = Nothing
    }
