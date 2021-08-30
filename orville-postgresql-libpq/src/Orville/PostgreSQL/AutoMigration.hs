{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.AutoMigration
  ( autoMigrateSchema,
    generateMigrationSteps,
    executeMigrationSteps,
    SchemaItem,
    schemaTable,
    MigrationDataError,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.String as String

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.InformationSchema as IS
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

{- |
  A 'SchemaItem' represents a single item in a database schema such as a table,
  index or constraint. The constructor functions below can be used to create
  items from other types (such as 'Orville.TableDefinition') to put them into
  a list to be used with 'autoMigrateSchema'.
-}
data SchemaItem where
  SchemaTable ::
    Orville.TableDefinition key writeEntity readEntity ->
    SchemaItem

{- |
  Constructs a 'SchemaItem' from a 'Orville.TableDefinition'.
-}
schemaTable ::
  Orville.TableDefinition key writeEntity readEntity ->
  SchemaItem
schemaTable =
  SchemaTable

{- |
  A single SQL statement that will be executed in order to migrate the database
  to the desired result. You can use 'generateMigrationSteps' to get a list
  of these yourself for inspection and debugging.
-}
newtype MigrationStep
  = MigrationStep RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A 'MigrationDataError' will be thrown from the migration functions if data
  necessary for migration cannot be found.
-}
data MigrationDataError
  = UnableToDiscoverCurrentSchema String
  deriving (Show)

instance Exception MigrationDataError

{- |
  An Map of tables discovered to exist in the database, keyed by schema and
  table name for easy lookup for generation migration steps.
-}
type TableIndex =
  Map.Map (IS.SchemaName, IS.TableName) IS.InformationSchemaTable

{- |
  This function compares the list of 'SchemaItem's provided against the current
  schema found in the database to determine whether any migration are
  necessary.  If any changes need to be made, this function executes. You can
  call 'generateMigrationSteps' and 'executeMigrationSteps' yourself if you
  want to have more control over the process.
-}
autoMigrateSchema :: Orville.MonadOrville m => [SchemaItem] -> m ()
autoMigrateSchema schemaItems = do
  steps <- generateMigrationSteps schemaItems
  executeMigrationSteps steps

{- |
  Compares the list of 'SchemaItem's provided against the current schema
  found in the database and returns a list of 'MigrationStep's that could be
  executed to make the database schema match the items given.

  You can execute the 'MigrationStep's yourself using 'Orville.executeVoid',
  or use the 'executeMigrationSteps' convenience function.
-}
generateMigrationSteps :: Orville.MonadOrville m => [SchemaItem] -> m [MigrationStep]
generateMigrationSteps schemaItems = do
  (currentSchema, currentCatalog) <- findCurrentSchema

  let schemaNames = databaseSchemaNames currentSchema schemaItems

  tableIndex <-
    indexExistingTables <$> findTablesInSchemas currentCatalog schemaNames

  pure $
    concatMap (calculateMigrationSteps currentSchema tableIndex) schemaItems

{- |
  A convenience function for executing a list of 'MigrationStep's that has
  be previously devised via 'generateMigrationSteps'.
-}
executeMigrationSteps :: Orville.MonadOrville m => [MigrationStep] -> m ()
executeMigrationSteps =
  traverse_ Orville.executeVoid

calculateMigrationSteps ::
  IS.SchemaName ->
  TableIndex ->
  SchemaItem ->
  [MigrationStep]
calculateMigrationSteps currentSchema tableIndex schemaItem =
  case schemaItem of
    SchemaTable tableDef ->
      let schemaName =
            tableDefinitionActualSchemaName currentSchema tableDef

          tableName =
            String.fromString $ Orville.unqualifiedTableNameString tableDef
       in case Map.lookup (schemaName, tableName) tableIndex of
            Nothing ->
              [MigrationStep . RawSql.toRawSql . Orville.mkCreateTableExpr $ tableDef]
            Just _ ->
              []

indexExistingTables :: [IS.InformationSchemaTable] -> TableIndex
indexExistingTables =
  let tableEntry table =
        ( (IS.tableSchema table, IS.tableName table)
        , table
        )
   in Map.fromList . map tableEntry

databaseSchemaNames :: IS.SchemaName -> [SchemaItem] -> Set.Set IS.SchemaName
databaseSchemaNames currentSchema =
  Set.fromList . map (schemaItemSchemaName currentSchema)

schemaItemSchemaName :: IS.SchemaName -> SchemaItem -> IS.SchemaName
schemaItemSchemaName currentSchema item =
  case item of
    SchemaTable tableDef ->
      tableDefinitionActualSchemaName currentSchema tableDef

tableDefinitionActualSchemaName ::
  IS.SchemaName ->
  Orville.TableDefinition key writeEntity readEntity ->
  IS.SchemaName
tableDefinitionActualSchemaName currentSchema tableDef =
  maybe currentSchema String.fromString (Orville.tableSchemaNameString tableDef)

currentSchemaQuery :: Expr.QueryExpr
currentSchemaQuery =
  Expr.queryExpr
    (Expr.selectClause (Expr.selectExpr Nothing))
    ( Expr.selectDerivedColumns
        [ Expr.deriveColumn (Orville.fieldColumnName currentSchemaField)
        , -- Without an an explicit "AS" here, postgresql returns a column name of
          -- "current_database" rather than "current_catalog"
          Expr.deriveColumnAs
            (Orville.fieldColumnName currentCatalogField)
            (Orville.fieldColumnName currentCatalogField)
        ]
    )
    Nothing

currentCatalogField :: Orville.FieldDefinition Orville.NotNull IS.CatalogName
currentCatalogField =
  Orville.coerceField
    (Orville.unboundedTextField "current_catalog")

currentSchemaField :: Orville.FieldDefinition Orville.NotNull IS.SchemaName
currentSchemaField =
  Orville.coerceField
    (Orville.unboundedTextField "current_schema")

findCurrentSchema :: Orville.MonadOrville m => m (IS.SchemaName, IS.CatalogName)
findCurrentSchema = do
  results <-
    Orville.executeAndDecode
      currentSchemaQuery
      ( (,)
          <$> Orville.marshallField fst currentSchemaField
          <*> Orville.marshallField snd currentCatalogField
      )

  liftIO $
    case results of
      [schemaAndCatalog] ->
        pure schemaAndCatalog
      [] ->
        throwIO $ UnableToDiscoverCurrentSchema "No results returned by current_schema query"
      _ ->
        throwIO $ UnableToDiscoverCurrentSchema "Multiple results returned by current_schema query"

findTablesInSchemas ::
  Orville.MonadOrville m =>
  IS.CatalogName ->
  Set.Set IS.SchemaName ->
  m [IS.InformationSchemaTable]
findTablesInSchemas currentCatalog schemaNameSet =
  case NEL.nonEmpty (Set.toList schemaNameSet) of
    Nothing ->
      pure []
    Just nonEmptySchemaNames ->
      Orville.findEntitiesBy
        IS.informationSchemaTablesTable
        ( Orville.where_
            ( Orville.whereAnd
                ( Orville.fieldEquals IS.tableCatalogField currentCatalog
                    :| [Orville.whereIn IS.tableSchemaField nonEmptySchemaNames]
                )
            )
        )
