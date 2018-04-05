{-|
Module    : Database.Orville.Internal.MigrateTable
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.Internal.MigrateTable
  ( createTable
  , dropTable
  , migrateTable
  , MigrateTableException(..)
  ) where

import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as List
import Data.Maybe
import Data.Typeable
import Database.HDBC

import Database.Orville.Internal.Execute
import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.Monad
import Database.Orville.Internal.Types

createTable ::
     MonadOrville conn m => conn -> TableDefinition entity key -> m ()
createTable conn tableDef = do
  let ddl = mkCreateTableDDL tableDef
  executingSql DDLQuery ddl $ void $ run conn ddl []

dropTable :: MonadOrville conn m => conn -> String -> m ()
dropTable conn name = do
  let ddl = "DROP TABLE \"" ++ name ++ "\""
  executingSql DDLQuery ddl $ void $ run conn ddl []

migrateTable ::
     MonadOrville conn m => conn -> TableDefinition entity key -> m ()
migrateTable conn tableDef = do
  columns <- liftIO $ describeTable conn (tableName tableDef)
  case mkMigrateTableDDL columns tableDef of
    Nothing -> return ()
    Just ddl -> do
      executingSql DDLQuery ddl $ do
        stmt <- prepare conn ddl
        executeRaw stmt `Exc.catch` (Exc.throw . MTE tableDef)

mkMigrateTableDDL ::
     [(String, SqlColDesc)] -> TableDefinition entity key -> Maybe String
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
             " SET DATA TYPE " ++ mkTypeDDL (fieldType fieldDef)
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
mkFlagDDL Null = Just "NULL"
mkFlagDDL (Default def) = Just $ "DEFAULT " ++ toColumnDefaultSql def
mkFlagDDL (References table field) =
  Just $ "REFERENCES \"" ++ tableName table ++ "\" (" ++ fieldName field ++ ")"
mkFlagDDL (ColumnDescription _) = Nothing

mkTypeDDL :: ColumnType -> String
mkTypeDDL AutomaticId = "SERIAL"
mkTypeDDL ForeignId = "INTEGER"
mkTypeDDL Integer = "INTEGER"
mkTypeDDL BigInteger = "BIGINT"
mkTypeDDL Double = "DOUBLE PRECISION"
mkTypeDDL Boolean = "BOOLEAN"
mkTypeDDL (Text len) = "CHAR(" ++ show len ++ ")"
mkTypeDDL (VarText len) = "VARCHAR(" ++ show len ++ ")"
mkTypeDDL (Date) = "DATE"
mkTypeDDL (Timestamp) = "TIMESTAMP with time zone"
mkTypeDDL TextSearchVector = "TSVECTOR"

mkFieldDDL :: FieldDefinition a -> String
mkFieldDDL (name, columnType, flags, _) =
  name ++ " " ++ sqlType ++ " " ++ flagSql
  where
    sqlType = mkTypeDDL columnType
    flagSql = List.intercalate " " (notNull : mapMaybe mkFlagDDL flags)
    notNull =
      if any isNullFlag flags
        then ""
        else "NOT NULL"

mkCreateTableDDL :: TableDefinition entity key -> String
mkCreateTableDDL tableDef =
  "CREATE TABLE \"" ++ tableName tableDef ++ "\" (" ++ fields ++ ")"
  where
    fields = List.intercalate ", " $ map mkSomeFieldDDL (tableFields tableDef)
    mkSomeFieldDDL (SomeField f) = mkFieldDDL f

columnTypeSqlId :: ColumnType -> SqlTypeId
columnTypeSqlId AutomaticId = SqlBigIntT
columnTypeSqlId ForeignId = SqlBigIntT
columnTypeSqlId Integer = SqlBigIntT
columnTypeSqlId Boolean = SqlBitT
columnTypeSqlId BigInteger = SqlBigIntT
columnTypeSqlId Double = SqlFloatT
columnTypeSqlId (VarText _) = SqlVarCharT
columnTypeSqlId (Text _) = SqlCharT
columnTypeSqlId Date = SqlDateT
columnTypeSqlId Timestamp = SqlTimestampWithZoneT
columnTypeSqlId TextSearchVector = SqlUnknownT "3614"

columnTypeSqlSize :: ColumnType -> Maybe Int
columnTypeSqlSize AutomaticId = Just 4
columnTypeSqlSize ForeignId = Just 4
columnTypeSqlSize Integer = Just 4
columnTypeSqlSize BigInteger = Just 8
columnTypeSqlSize Double = Just 8
columnTypeSqlSize Boolean = Just 1
columnTypeSqlSize (VarText n) = Just n
columnTypeSqlSize (Text n) = Just n
columnTypeSqlSize Date = Just 4
columnTypeSqlSize Timestamp = Just 8
columnTypeSqlSize TextSearchVector = Nothing

sqlFieldDesc :: FieldDefinition a -> SqlColDesc
sqlFieldDesc (_, columnType, flags, _) =
  SqlColDesc
    { colType = columnTypeSqlId columnType
    , colSize = columnTypeSqlSize columnType
    , colNullable = Just (any isNullFlag flags)
    , colOctetLength = Nothing
    , colDecDigits = Nothing
    }

data MigrateTableException =
  forall entity key. MTE (TableDefinition entity key)
                         Exc.SomeException
  deriving (Typeable)

instance Show MigrateTableException where
  show = formatMigrationException

instance Exc.Exception MigrateTableException

formatMigrationException :: MigrateTableException -> String
formatMigrationException (MTE tableDef exception) = message
  where
    message =
      "There was an error migrating table " ++
      name ++
      ".\n\
                  \The error is:\n\
                  \\n\
                  \ " ++
      Exc.displayException exception ++
      "\\n\
                  \\n\
                  \\n\
                  \Here are the developer comments regarding the table:\n\
                  \\n\
                  \ " ++
      comments ++
      "\
                  \\n"
    name = tableName tableDef
    comments = formatTableComments " " tableDef

formatTableComments :: String -> TableDefinition entity key -> String
formatTableComments indent tableDef =
  List.intercalate ("\n" ++ indent) commentLines
  where
    commentLines = map formatTableComment comments
    comments = runComments (tableComments tableDef)

formatTableComment :: TableComment -> String
formatTableComment c =
  List.intercalate " - " [tcWhat c, show (tcWhen c), tcWho c]
