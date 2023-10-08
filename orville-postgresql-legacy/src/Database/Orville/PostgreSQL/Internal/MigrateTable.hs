{-|
Module    : Database.Orville.PostgreSQL.Internal.MigrateTable
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.PostgreSQL.Internal.MigrateTable
  ( migrateTablePlan
  , dropTablePlan
  ) where

import Control.Monad
import qualified Data.List as List
import Data.Maybe
import Database.HDBC

import Database.Orville.PostgreSQL.Internal.Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.MigrationPlan
import Database.Orville.PostgreSQL.Internal.PrimaryKey
import Database.Orville.PostgreSQL.Internal.SchemaState
import Database.Orville.PostgreSQL.Internal.SqlType
import Database.Orville.PostgreSQL.Internal.Types

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
    fieldNamesToDelete = tableSafeToDelete tableDef
    fieldColumn fieldDef = lookup (fieldName fieldDef) columns
    colStmt (SomeField f) = mkMigrateColumnDDL f (fieldColumn f)
    dropStmt name = mkDropColumnDDL name (lookup name columns)
    stmts =
      List.concatMap colStmt fields ++
      List.concatMap dropStmt fieldNamesToDelete
    cols = List.intercalate ", " $ stmts

mkMigrateColumnTypeDDL :: FieldDefinition nullability a
                       -> SqlColDesc
                       -> Maybe String
mkMigrateColumnTypeDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
      name = rawExprToSql . generateSql . NameForm Nothing $ fieldName fieldDef
   in if colType fieldDesc /= colType colDesc ||
         colSize fieldDesc /= colSize colDesc
        then Just $
             "ALTER COLUMN " ++
             name ++
             " SET DATA TYPE " ++ sqlTypeDDL (fieldType fieldDef)
        else Nothing

mkMigrateColumnNullDDL :: FieldDefinition nullability a
                       -> SqlColDesc
                       -> Maybe String
mkMigrateColumnNullDDL fieldDef colDesc =
  let fieldDesc = sqlFieldDesc fieldDef
      fieldNull = fromMaybe True (colNullable fieldDesc)
      colNull = fromMaybe True (colNullable colDesc)
      name = rawExprToSql . generateSql . NameForm Nothing $ fieldName fieldDef
   in if fieldNull && not colNull
        then Just $ "ALTER COLUMN " ++ name ++ " DROP NOT NULL"
        else if not fieldNull && colNull
               then Just $
                    "ALTER COLUMN " ++ name ++ " SET NOT NULL"
               else Nothing

mkMigrateColumnDDL :: FieldDefinition nullability a
                   -> Maybe SqlColDesc
                   -> [String]
mkMigrateColumnDDL fieldDef Nothing = ["ADD COLUMN " ++ mkFieldDDL fieldDef]
mkMigrateColumnDDL fieldDef (Just desc) =
  catMaybes
    [ mkMigrateColumnTypeDDL fieldDef desc
    , mkMigrateColumnNullDDL fieldDef desc
    ]

mkDropColumnDDL :: String -> Maybe SqlColDesc -> [String]
mkDropColumnDDL _ Nothing = []
mkDropColumnDDL name (Just _) = ["DROP COLUMN " ++ (rawExprToSql . generateSql . NameForm Nothing) name]

mkFlagDDL :: ColumnFlag -> Maybe String
mkFlagDDL Unique = Just "UNIQUE"
mkFlagDDL (Default def) = Just $ "DEFAULT " ++ toColumnDefaultSql def
mkFlagDDL (References table field) =
  Just $ "REFERENCES \"" ++ tableName table ++ "\" (" ++ (rawExprToSql . generateSql . NameForm Nothing . fieldName) field ++ ")"
mkFlagDDL (ColumnDescription _) = Nothing
mkFlagDDL AssignedByDatabase = Nothing

mkFieldDDL :: FieldDefinition nullability a -> String
mkFieldDDL field = name ++ " " ++ sqlType ++ " " ++ flagSql
  where
    name = rawExprToSql . generateSql . fieldToNameForm $ field
    sqlType = sqlTypeDDL (fieldType field)
    flagSql =
      List.intercalate " " (notNull : mapMaybe mkFlagDDL (fieldFlags field))
    notNull =
      if isFieldNullable field
        then "NULL"
        else "NOT NULL"

mkPrimaryKeyDDL :: PrimaryKey key -> String
mkPrimaryKeyDDL keyDef =
  let
    names =
      mapPrimaryKeyParts (\_ field -> fieldToNameForm field) keyDef
  in
    concat
      [ "PRIMARY KEY ("
      , List.intercalate ", " (map (rawExprToSql . generateSql) names)
      , ")"
      ]

mkCreateTableDDL :: TableDefinition readEntity writeEntity key -> String
mkCreateTableDDL tableDef =
  let
    mkSomeFieldDDL (SomeField f) =
      mkFieldDDL f

    fields =
      List.intercalate ", " $ map mkSomeFieldDDL (tableFields tableDef)

    primaryKeyDDL =
      mkPrimaryKeyDDL (tablePrimaryKey tableDef)
  in
    concat
      [ "CREATE TABLE \""
      , tableName tableDef
      , "\" ("
      , fields
      , ", "
      , primaryKeyDDL
      , ")"
      ]

mkDropTableDDL :: String -> String
mkDropTableDDL name = "DROP TABLE \"" ++ name ++ "\""

sqlFieldDesc :: FieldDefinition nullability a -> SqlColDesc
sqlFieldDesc field =
  SqlColDesc
    { colType = sqlTypeId $ fieldType field
    , colSize = sqlTypeSqlSize $ fieldType field
    , colNullable = Just (isFieldNullable field)
    , colOctetLength = Nothing
    , colDecDigits = Nothing
    }
