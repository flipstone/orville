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

import Control.Exception.Safe (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import qualified Data.String as String

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.InformationSchema as IS
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MigrationLock as MigrationLock
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
  Map.Map (IS.SchemaName, IS.TableName) (Set.Set IS.ColumnName)

{- |
  This function compares the list of 'SchemaItem's provided against the current
  schema found in the database to determine whether any migration are
  necessary.  If any changes need to be made, this function executes. You can
  call 'generateMigrationSteps' and 'executeMigrationSteps' yourself if you
  want to have more control over the process.
-}
autoMigrateSchema :: Orville.MonadOrville m => [SchemaItem] -> m ()
autoMigrateSchema schemaItems = do
  MigrationLock.withLockedTransaction $ do
    steps <- generateMigrationStepsWithoutTransaction schemaItems
    executeMigrationStepsWithoutTransaction steps

{- |
  Compares the list of 'SchemaItem's provided against the current schema
  found in the database and returns a list of 'MigrationStep's that could be
  executed to make the database schema match the items given.

  You can execute the 'MigrationStep's yourself using 'Orville.executeVoid',
  or use the 'executeMigrationSteps' convenience function.
-}
generateMigrationSteps :: Orville.MonadOrville m => [SchemaItem] -> m [MigrationStep]
generateMigrationSteps =
  MigrationLock.withLockedTransaction . generateMigrationStepsWithoutTransaction

generateMigrationStepsWithoutTransaction :: Orville.MonadOrville m => [SchemaItem] -> m [MigrationStep]
generateMigrationStepsWithoutTransaction schemaItems = do
  (currentSchema, currentCatalog) <- findCurrentSchema

  let schemaNames = databaseSchemaNames currentSchema schemaItems

  tables <- findTablesInSchemas currentCatalog schemaNames
  tablesWithColumns <- traverse findTableColumns tables

  let tableIndex =
        indexExistingTables tablesWithColumns

  pure $
    concatMap (calculateMigrationSteps currentSchema tableIndex) schemaItems

{- |
  A convenience function for executing a list of 'MigrationStep's that has
  be previously devised via 'generateMigrationSteps'.
-}
executeMigrationSteps :: Orville.MonadOrville m => [MigrationStep] -> m ()
executeMigrationSteps =
  MigrationLock.withLockedTransaction . executeMigrationStepsWithoutTransaction

executeMigrationStepsWithoutTransaction :: Orville.MonadOrville m => [MigrationStep] -> m ()
executeMigrationStepsWithoutTransaction =
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
              [mkCreateTableStep tableDef]
            Just existingColumns ->
              maybeToList $ mkAlterTableStep existingColumns tableDef

{- |
  Builds a 'MigrationStep' that will perform table creation. This function
  assumes the table does not exist. The migration step it produces will fail
  if the table already exists in its schema.
-}
mkCreateTableStep ::
  Orville.TableDefinition key writeEntity readEntity ->
  MigrationStep
mkCreateTableStep =
  MigrationStep . RawSql.toRawSql . Orville.mkCreateTableExpr

{- |
  Builds a 'MigrationStep' that will perform a table alternation, or none if no
  alteration is required. The column names given are expected to reflect that
  that currently exist in the schema. This function compare thes ecolumns to
  the given table definition to determine what alterations need to be
  performed. If there is nothing to do, 'Nothing' will be returned.
-}
mkAlterTableStep ::
  Set.Set IS.ColumnName ->
  Orville.TableDefinition key writeEntity readEntity ->
  Maybe MigrationStep
mkAlterTableStep existingColumns tableDef =
  let addFieldAction ::
        Orville.FieldDefinition nullability a ->
        accessor ->
        [Expr.AlterTableAction] ->
        [Expr.AlterTableAction]
      addFieldAction fieldDef _ actions =
        case mkAlterTableAction existingColumns fieldDef of
          Nothing ->
            actions
          Just newAction ->
            newAction : actions

      tableActions =
        Orville.foldMarshallerFields
          (Orville.tableMarshaller tableDef)
          []
          addFieldAction
   in fmap
        (MigrationStep . RawSql.toRawSql . Expr.alterTableExpr (Orville.tableName tableDef))
        (nonEmpty tableActions)

{- |
  Builds an 'Expr.AlterTableAction' expression for the given
  'Orville.FieldDefinition', or none if no change is required.
-}
mkAlterTableAction ::
  Set.Set IS.ColumnName ->
  Orville.FieldDefinition nullability a ->
  Maybe Expr.AlterTableAction
mkAlterTableAction existingColumns fieldDef =
  let isColumnName =
        String.fromString (Orville.fieldNameToString $ Orville.fieldName fieldDef)
   in if Set.member isColumnName existingColumns
        then Nothing
        else Just $ Expr.addColumn (Orville.fieldColumnDefinition fieldDef)

indexExistingTables :: [(IS.InformationSchemaTable, [IS.InformationSchemaColumn])] -> TableIndex
indexExistingTables =
  let tableEntry (table, columns) =
        ( (IS.tableSchema table, IS.tableName table)
        , Set.fromList (map IS.columnName columns)
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
        [ Expr.deriveColumn (unquotedFieldName currentSchemaField)
        , -- Without an an explicit "AS" here, postgresql returns a column name of
          -- "current_database" rather than "current_catalog"
          Expr.deriveColumnAs
            (unquotedFieldName currentCatalogField)
            (Orville.fieldColumnName currentCatalogField)
        ]
    )
    Nothing

{- |
  current_schema and current_catalog are special words -- if you put them in
  quotes PostgreSQL treats them as a normal column name. These column don't
  exist in our query above, so it would fail. We use this function to include
  them in the SQL unquoted.
-}
unquotedFieldName :: Orville.FieldDefinition nullability a -> Expr.ColumnName
unquotedFieldName =
  Expr.columnNameFromIdentifier
    . Expr.unquotedIdentifierFromBytes
    . Orville.fieldNameToByteString
    . Orville.fieldName

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

findTableColumns ::
  Orville.MonadOrville m =>
  IS.InformationSchemaTable ->
  m (IS.InformationSchemaTable, [IS.InformationSchemaColumn])
findTableColumns table = do
  columns <- IS.describeTableColumns table
  pure (table, columns)
