{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.AutoMigration
  ( autoMigrateSchema,
    generateMigrationSteps,
    executeMigrationSteps,
    SchemaItem,
    schemaTable,
    MigrationStep,
    MigrationDataError,
  )
where

import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (maybeToList)
import qualified Data.String as String

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MigrationLock as MigrationLock
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

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
  currentNamespace <- findCurrentNamespace

  let pgCatalogRelations = fmap (schemaItemPgCatalogRelation currentNamespace) schemaItems

  dbDesc <- PgCatalog.describeDatabaseRelations pgCatalogRelations

  pure $
    concatMap (calculateMigrationSteps currentNamespace dbDesc) schemaItems

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
  PgCatalog.NamespaceName ->
  PgCatalog.DatabaseDescription ->
  SchemaItem ->
  [MigrationStep]
calculateMigrationSteps currentNamespace dbDesc schemaItem =
  case schemaItem of
    SchemaTable tableDef ->
      let schemaName =
            tableDefinitionActualNamespaceName currentNamespace tableDef

          tableName =
            String.fromString $ Orville.unqualifiedTableNameString tableDef
       in case PgCatalog.lookupRelation (schemaName, tableName) dbDesc of
            Nothing ->
              [mkCreateTableStep tableDef]
            Just relationDesc ->
              maybeToList $ mkAlterTableStep relationDesc tableDef

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
  PgCatalog.RelationDescription ->
  Orville.TableDefinition key writeEntity readEntity ->
  Maybe MigrationStep
mkAlterTableStep relationDesc tableDef =
  let addFieldAction ::
        Orville.FieldDefinition nullability a ->
        accessor ->
        [Expr.AlterTableAction] ->
        [Expr.AlterTableAction]
      addFieldAction fieldDef _ actions =
        mkAlterTableActions relationDesc fieldDef <> actions

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
mkAlterTableActions ::
  PgCatalog.RelationDescription ->
  Orville.FieldDefinition nullability a ->
  [Expr.AlterTableAction]
mkAlterTableActions relationDesc fieldDef =
  let pgAttributeName =
        String.fromString (Orville.fieldNameToString $ Orville.fieldName fieldDef)
   in case PgCatalog.lookupAttribute pgAttributeName relationDesc of
        Just attr
          | PgCatalog.isOrdinaryColumn attr ->
            let sqlType =
                  Orville.fieldType fieldDef

                typeIsChanged =
                  (Orville.sqlTypeOid sqlType /= PgCatalog.pgAttributeTypeOid attr)
                    || (Orville.sqlTypeMaximumLength sqlType /= PgCatalog.pgAttributeMaxLength attr)

                columnName =
                  Orville.fieldColumnName fieldDef

                dataType =
                  Orville.sqlTypeExpr sqlType

                alterType = do
                  guard typeIsChanged
                  [Expr.alterColumnType columnName dataType (Just $ Expr.usingCast columnName dataType)]

                nullabilityIsChanged =
                  Orville.fieldIsNotNull fieldDef /= PgCatalog.pgAttributeIsNotNull attr

                nullabilityAction =
                  if Orville.fieldIsNotNull fieldDef
                    then Expr.setNotNull
                    else Expr.dropNotNull

                alterNullability = do
                  guard nullabilityIsChanged
                  [Expr.alterColumnNullability (Orville.fieldColumnName fieldDef) nullabilityAction]
             in alterType <> alterNullability
        _ ->
          -- Either the column doesn't exist in the table _OR_ it's a system
          -- column. If it's a system column, attempting to add it will result
          -- in an error that will be reported to the user. We could explicitly
          -- return an error from this function, but that would make the error
          -- reporting inconsistent with the handling in create table, where we
          -- must rely on the database to raise the error because the table
          -- does not yet exist for us to discover a conflict with system
          -- attributes.
          [Expr.addColumn (Orville.fieldColumnDefinition fieldDef)]

schemaItemPgCatalogRelation ::
  PgCatalog.NamespaceName ->
  SchemaItem ->
  (PgCatalog.NamespaceName, PgCatalog.RelationName)
schemaItemPgCatalogRelation currentNamespace item =
  case item of
    SchemaTable tableDef ->
      ( tableDefinitionActualNamespaceName currentNamespace tableDef
      , String.fromString (Orville.unqualifiedTableNameString tableDef)
      )

tableDefinitionActualNamespaceName ::
  PgCatalog.NamespaceName ->
  Orville.TableDefinition key writeEntity readEntity ->
  PgCatalog.NamespaceName
tableDefinitionActualNamespaceName currentNamespace tableDef =
  maybe currentNamespace String.fromString (Orville.tableSchemaNameString tableDef)

currentNamespaceQuery :: Expr.QueryExpr
currentNamespaceQuery =
  Expr.queryExpr
    (Expr.selectClause (Expr.selectExpr Nothing))
    ( Expr.selectDerivedColumns
        [ Expr.deriveColumnAs
            -- current_schema is a special reserved word in postgresql. If you
            -- put it in quotes it tries to treat it as a regular column name,
            -- which then can't be found as a column in the query.
            (Expr.columnNameFromIdentifier (Expr.unquotedIdentifier "current_schema"))
            (Orville.fieldColumnName PgCatalog.namespaceNameField)
        ]
    )
    Nothing

findCurrentNamespace :: Orville.MonadOrville m => m PgCatalog.NamespaceName
findCurrentNamespace = do
  results <-
    Orville.executeAndDecode
      currentNamespaceQuery
      (Orville.marshallField id PgCatalog.namespaceNameField)

  liftIO $
    case results of
      [schemaAndCatalog] ->
        pure schemaAndCatalog
      [] ->
        throwIO $ UnableToDiscoverCurrentSchema "No results returned by current_schema query"
      _ ->
        throwIO $ UnableToDiscoverCurrentSchema "Multiple results returned by current_schema query"
