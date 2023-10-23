{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Facilities for performing some database migrations automatically.
See 'autoMigrateSchema' as a primary, high-level entry point.

@since 1.0.0.0
-}
module Orville.PostgreSQL.AutoMigration
  ( MigrationOptions (runSchemaChanges, runConcurrentIndexCreations, migrationLockId)
  , defaultOptions
  , autoMigrateSchema
  , SchemaItem (..)
  , schemaItemSummary
  , MigrationPlan
  , generateMigrationPlan
  , migrationPlanSteps
  , executeMigrationPlan
  , MigrationStep
  , MigrationDataError
  , MigrationLock.MigrationLockId
  , MigrationLock.defaultLockId
  , MigrationLock.nextLockId
  , MigrationLock.withMigrationLock
  , MigrationLock.MigrationLockError
  )
where

import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (guard, when)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text.Encoding as Enc
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.IndexDefinition as IndexDefinition
import qualified Orville.PostgreSQL.Internal.MigrationLock as MigrationLock
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema as Schema

{- |
  A 'SchemaItem' represents a single item in a database schema such as a table,
  index or constraint. The constructor functions below can be used to create
  items from other types (such as 'Orville.TableDefinition') to put them into
  a list to be used with 'autoMigrateSchema'.

@since 1.0.0.0
-}
data SchemaItem where
  -- |
  --    Constructs a 'SchemaItem' from a 'Orville.TableDefinition'.
  -- @since 1.0.0.0
  SchemaTable ::
    Orville.TableDefinition key writeEntity readEntity ->
    SchemaItem
  -- |
  --    Constructs a 'SchemaItem' that will drop the specified table if it is
  --    found in the database.
  -- @since 1.0.0.0
  SchemaDropTable ::
    Orville.TableIdentifier ->
    SchemaItem
  -- |
  --    Constructs a 'SchemaItem' from a 'Orville.SequenceDefinition'.
  -- @since 1.0.0.0
  SchemaSequence ::
    Orville.SequenceDefinition ->
    SchemaItem
  -- |
  --    Constructs a 'SchemaItem' that will drop the specified table if it is
  --    found in the database.
  -- @since 1.0.0.0
  SchemaDropSequence ::
    Orville.SequenceIdentifier ->
    SchemaItem

{- |
  Returns a one-line string describing the 'SchemaItem', suitable for a human
  to identify it in a list of output.

  For example, a 'SchemaItem' constructed via 'SchemaTable' gives @Table <table
  name>@.

@since 1.0.0.0
-}
schemaItemSummary :: SchemaItem -> String
schemaItemSummary item =
  case item of
    SchemaTable tableDef ->
      "Table " <> Orville.tableIdToString (Orville.tableIdentifier tableDef)
    SchemaDropTable tableId ->
      "Drop table " <> Orville.tableIdToString tableId
    SchemaSequence sequenceDef ->
      "Sequence " <> Orville.sequenceIdToString (Orville.sequenceIdentifier sequenceDef)
    SchemaDropSequence sequenceId ->
      "Drop sequence " <> Orville.sequenceIdToString sequenceId

{- |
A 'MigrationPlan' contains an ordered list of migration steps. Each one is a
single DDL statement to make a specific database change. The steps are ordered
such that dependencies from earlier steps will be in place before a later step
is executed (e.g. new columns are added before foreign keys referring to them).

While most steps are executed together in a single transaction this is not
possible for indexes being created concurrently. Any such steps are executed
last after the transaction for the rest of the schema changes has been
successfully committed.

@since 1.0.0.0
-}
data MigrationPlan = MigrationPlan
  { i_transactionalSteps :: [MigrationStep]
  , i_concurrentIndexSteps :: [MigrationStep]
  }

{- |
  Returns all the 'MigrationStep's found in a 'MigrationPlan' together in a
  single list. This is useful if you merely want to examine the steps of a plan
  rather than execute them. You should always use 'executeMigrationPlan' to
  execute a migration plan to ensure that the transactional steps are done
  within a transaction while the concurrent index steps are done afterward
  outside of it.

@since 1.0.0.0
-}
migrationPlanSteps :: MigrationPlan -> [MigrationStep]
migrationPlanSteps plan =
  i_transactionalSteps plan <> i_concurrentIndexSteps plan

mkMigrationPlan :: [MigrationStepWithType] -> MigrationPlan
mkMigrationPlan steps =
  let
    (transactionalSteps, concurrentIndexSteps) =
      List.partition isMigrationStepTransactional
        . List.sortOn migrationStepType
        $ steps
  in
    MigrationPlan
      { i_transactionalSteps = map migrationStep transactionalSteps
      , i_concurrentIndexSteps = map migrationStep concurrentIndexSteps
      }

{- |
  A single SQL statement that will be executed in order to migrate the database
  to the desired result. You can use 'generateMigrationPlan' to get a list
  of these yourself for inspection and debugging.

@since 1.0.0.0
-}
newtype MigrationStep
  = MigrationStep RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  This type is used internally by Orville to order the migration steps after
  they have been created. It is not exposed outside this module.

@since 1.0.0.0
-}
data MigrationStepWithType = MigrationStepWithType
  { migrationStepType :: StepType
  , migrationStep :: MigrationStep
  }

mkMigrationStepWithType ::
  RawSql.SqlExpression sql =>
  StepType ->
  sql ->
  MigrationStepWithType
mkMigrationStepWithType stepType sql =
  MigrationStepWithType
    { migrationStepType = stepType
    , migrationStep = MigrationStep (RawSql.toRawSql sql)
    }

isMigrationStepTransactional :: MigrationStepWithType -> Bool
isMigrationStepTransactional stepWithType =
  case migrationStepType stepWithType of
    DropForeignKeys -> True
    DropUniqueConstraints -> True
    DropIndexes -> True
    AddRemoveTablesAndColumns -> True
    AddIndexesTransactionally -> True
    AddUniqueConstraints -> True
    AddForeignKeys -> True
    AddIndexesConcurrently -> False

{- |
  Indicates the kind of operation being performed by a 'MigrationStep' so
  that the steps can be ordered in a sequence that is guaranteed to succeed.
  The order of the constructors below indicates the order in which steps will
  be run.

@since 1.0.0.0
-}
data StepType
  = DropForeignKeys
  | DropUniqueConstraints
  | DropIndexes
  | AddRemoveTablesAndColumns
  | AddIndexesTransactionally
  | AddUniqueConstraints
  | AddForeignKeys
  | AddIndexesConcurrently
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Ord
    )

{- |
  A 'MigrationDataError' will be thrown from the migration functions if data
  necessary for migration cannot be found.

@since 1.0.0.0
-}
data MigrationDataError
  = UnableToDiscoverCurrentSchema String
  | PgCatalogInvariantViolated String
  deriving
    ( -- | @since 1.0.0.0
      Show
    )

-- | @since 1.0.0.0
instance Exception MigrationDataError

{- |
Options to control how 'autoMigrateSchema' and similar functions behave. You
should use 'defaultOptions' to construct a 'MigrationOptions' value
and then use the record accessors to change any values you want to customize.

@since 1.0.0.0
-}
data MigrationOptions = MigrationOptions
  { runSchemaChanges :: Bool
  -- ^
  --       Indicates whether the normal schema changes (other than concurrent index
  --       creations) should be run. The default value is 'True'. You may want to
  --       disable this if you wish to run concurrent index creations separately
  --       from the rest of the schema changes.
  --
  --       @since 1.0.0.0
  , runConcurrentIndexCreations :: Bool
  -- ^
  --       Indicates whether indexes with the 'Orville.Concurrent' creation strategy
  --       will be created. The default value is 'True'. You may want to disable
  --       this if you wish to run concurrent index creations separately from the
  --       rest of the schema changes.
  --
  --       @since 1.0.0.0
  , migrationLockId :: MigrationLock.MigrationLockId
  -- ^
  --       The 'MigrationLock.MigrationLockId' that will be use to ensure only
  --       one application is running migrations at a time. The default value
  --       is 'MigrationLock.defaultLockId'. You may want to change this if you
  --       want to run concurrent index creations separately from the rest of
  --       the schema changes without blocking one another.
  --
  --       @since 1.0.0.0
  }

{- |
The default 'MigrationOptions', which is to run both the schema changes and
concurrent index creations together using the default Orville migration lock.

@since 1.0.0.0
-}
defaultOptions :: MigrationOptions
defaultOptions =
  MigrationOptions
    { runSchemaChanges = True
    , runConcurrentIndexCreations = True
    , migrationLockId = MigrationLock.defaultLockId
    }

{- |
  This function compares the list of 'SchemaItem's provided against the current
  schema found in the database to determine whether any migrations are
  necessary.  If any changes need to be made, this function executes. You can
  call 'generateMigrationPlan' and 'executeMigrationPlan' yourself if you want
  to have more control over the process, but must then take care to ensure that
  the schema has not changed between the two calls. This function uses a
  PostgreSQL advisory lock to ensure that no other calls to 'autoMigrateSchema'
  (potentially on other processes) attempt to modify the schema at the same
  time.

@since 1.0.0.0
-}
autoMigrateSchema ::
  Orville.MonadOrville m =>
  MigrationOptions ->
  [SchemaItem] ->
  m ()
autoMigrateSchema options schemaItems =
  MigrationLock.withMigrationLock (migrationLockId options) $ do
    plan <- generateMigrationPlanWithoutLock schemaItems
    executeMigrationPlanWithoutLock options plan

{- |
  Compares the list of 'SchemaItem's provided against the current schema found
  in the database and returns a 'MigrationPlan' that could be executed to make
  the database schema match the items given.

  You can execute the 'MigrationPlan' yourself using the 'executeMigrationPlan'
  convenience function, though 'autoMigrateSchema' is usually a better option
  because it uses a database lock to ensure that no other processes are also
  using 'autoMigrateSchema' to apply migrations at the same time. If you use
  'generateMigrationPlan' and 'executeMigrationPlan' separately, you are
  responsible for ensuring that the schema has not changed between the time the
  plan is generated and executed yourself.

@since 1.0.0.0
-}
generateMigrationPlan ::
  Orville.MonadOrville m =>
  MigrationOptions ->
  [SchemaItem] ->
  m MigrationPlan
generateMigrationPlan options =
  MigrationLock.withMigrationLock (migrationLockId options)
    . generateMigrationPlanWithoutLock

generateMigrationPlanWithoutLock :: Orville.MonadOrville m => [SchemaItem] -> m MigrationPlan
generateMigrationPlanWithoutLock schemaItems =
  Orville.withTransaction $ do
    currentNamespace <- findCurrentNamespace

    let
      pgCatalogRelations = fmap (schemaItemPgCatalogRelation currentNamespace) schemaItems

    dbDesc <- PgCatalog.describeDatabaseRelations pgCatalogRelations

    case traverse (calculateMigrationSteps currentNamespace dbDesc) schemaItems of
      Left err ->
        liftIO . throwIO $ err
      Right migrationSteps ->
        pure . mkMigrationPlan . concat $ migrationSteps

{- |
  Executes a 'MigrationPlan' that has been previously devised via
  'generateMigrationPlan'. Normally all the steps in a migration plan are
  executed in a transaction so that they will all be applied together
  successfully or all rolled-back if one of them fails. Any indexes using the
  'Orville.Concurrent' creation strategy cannot be created this way, however,
  because PostgreSQL does not allow @CREATE INDEX CONCURRENTLY@ to be used from
  inside a transaction. If a 'MigrationPlan' includes any indexes whose
  creation strategy is set to 'Orville.Concurrent', Orville will create indexes
  after the rest of the migration steps have been committed successfully. This
  function will wait until all of the migration steps that it runs to finish
  before returning. If one of the concurrent indexes fails during creation, it
  will be left in an invalid state (as is the default PostgreSQL behavior). You
  should check on the status of indexes created this way manually to ensure
  they were created successfully. If they could not be, you can drop them and
  Orville will re-attempt creating them the next time migration is performed.

@since 1.0.0.0
-}
executeMigrationPlan ::
  Orville.MonadOrville m =>
  MigrationOptions ->
  MigrationPlan ->
  m ()
executeMigrationPlan options =
  MigrationLock.withMigrationLock (migrationLockId options)
    . executeMigrationPlanWithoutLock options

executeMigrationPlanWithoutLock ::
  Orville.MonadOrville m =>
  MigrationOptions ->
  MigrationPlan ->
  m ()
executeMigrationPlanWithoutLock options plan = do
  when (runSchemaChanges options)
    . Orville.withTransaction
    . executeMigrationStepsWithoutTransaction
    . i_transactionalSteps
    $ plan

  when (runConcurrentIndexCreations options)
    . executeMigrationStepsWithoutTransaction
    $ i_concurrentIndexSteps
    $ plan

executeMigrationStepsWithoutTransaction :: Orville.MonadOrville m => [MigrationStep] -> m ()
executeMigrationStepsWithoutTransaction =
  traverse_ (Orville.executeVoid Orville.DDLQuery)

calculateMigrationSteps ::
  PgCatalog.NamespaceName ->
  PgCatalog.DatabaseDescription ->
  SchemaItem ->
  Either MigrationDataError [MigrationStepWithType]
calculateMigrationSteps currentNamespace dbDesc schemaItem =
  case schemaItem of
    SchemaTable tableDef ->
      Right $
        let
          (schemaName, tableName) =
            tableIdToPgCatalogNames
              currentNamespace
              (Orville.tableIdentifier tableDef)
        in
          case PgCatalog.lookupRelationOfKind PgCatalog.OrdinaryTable (schemaName, tableName) dbDesc of
            Nothing ->
              mkCreateTableSteps currentNamespace tableDef
            Just relationDesc ->
              mkAlterTableSteps currentNamespace relationDesc tableDef
    SchemaDropTable tableId ->
      Right $
        let
          (schemaName, tableName) =
            tableIdToPgCatalogNames currentNamespace tableId
        in
          case PgCatalog.lookupRelation (schemaName, tableName) dbDesc of
            Nothing ->
              []
            Just _ ->
              let
                dropTableExpr =
                  Expr.dropTableExpr
                    Nothing
                    (Orville.tableIdQualifiedName tableId)
              in
                [mkMigrationStepWithType AddRemoveTablesAndColumns dropTableExpr]
    SchemaSequence sequenceDef ->
      let
        (schemaName, sequenceName) =
          sequenceIdToPgCatalogNames
            currentNamespace
            (Orville.sequenceIdentifier sequenceDef)
      in
        case PgCatalog.lookupRelationOfKind PgCatalog.Sequence (schemaName, sequenceName) dbDesc of
          Nothing ->
            Right
              [ mkMigrationStepWithType
                  AddRemoveTablesAndColumns
                  (Orville.mkCreateSequenceExpr sequenceDef)
              ]
          Just relationDesc ->
            case PgCatalog.relationSequence relationDesc of
              Nothing ->
                Left . PgCatalogInvariantViolated $
                  "Sequence "
                    <> PgCatalog.namespaceNameToString schemaName
                    <> "."
                    <> PgCatalog.relationNameToString sequenceName
                    <> " was found in the 'pg_class' table but no corresponding 'pg_sequence' row was found"
              Just pgSequence ->
                Right $
                  mkAlterSequenceSteps sequenceDef pgSequence
    SchemaDropSequence sequenceId ->
      Right $
        let
          (schemaName, sequenceName) =
            sequenceIdToPgCatalogNames currentNamespace sequenceId
        in
          case PgCatalog.lookupRelationOfKind PgCatalog.Sequence (schemaName, sequenceName) dbDesc of
            Nothing ->
              []
            Just _ ->
              [ mkMigrationStepWithType
                  AddRemoveTablesAndColumns
                  (Expr.dropSequenceExpr Nothing (Orville.sequenceIdQualifiedName sequenceId))
              ]

{- |
  Builds 'MigrationStep's that will perform table creation. This function
  assumes the table does not exist. The migration step it produces will fail if
  the table already exists in its schema. Multiple steps may be required to
  create the table if foreign keys exist to that reference other tables, which
  may not have been created yet.

@since 1.0.0.0
-}
mkCreateTableSteps ::
  PgCatalog.NamespaceName ->
  Orville.TableDefinition key writeEntity readEntity ->
  [MigrationStepWithType]
mkCreateTableSteps currentNamespace tableDef =
  let
    tableName =
      Orville.tableName tableDef

    -- constraints are not included in the create table expression because
    -- they are added in a separate migration step to avoid ordering problems
    -- when creating multiple tables with interrelated foreign keys.
    createTableExpr =
      Expr.createTableExpr
        tableName
        (Orville.mkTableColumnDefinitions tableDef)
        (Orville.mkTablePrimaryKeyExpr tableDef)
        []

    addConstraintActions =
      concatMap
        (mkAddConstraintActions currentNamespace Set.empty)
        (Schema.tableConstraintDefinitions $ Orville.tableConstraints tableDef)

    addIndexSteps =
      concatMap
        (mkAddIndexSteps Set.empty tableName)
        (Orville.tableIndexes tableDef)
  in
    mkMigrationStepWithType AddRemoveTablesAndColumns createTableExpr
      : mkConstraintSteps tableName addConstraintActions
        <> addIndexSteps

{- |
  Builds migration steps that are required to create or alter the table's
  schema to make it match the given table definition.

  This function uses the given relation description to determine what
  alterations need to be performed. If there is nothing to do, an empty list
  will be returned.

@since 1.0.0.0
-}
mkAlterTableSteps ::
  PgCatalog.NamespaceName ->
  PgCatalog.RelationDescription ->
  Orville.TableDefinition key writeEntity readEntity ->
  [MigrationStepWithType]
mkAlterTableSteps currentNamespace relationDesc tableDef =
  let
    addAlterColumnActions =
      concat $
        Orville.foldMarshallerFields
          (Orville.unannotatedSqlMarshaller $ Orville.tableMarshaller tableDef)
          []
          (Orville.collectFromField Orville.IncludeReadOnlyColumns (mkAddAlterColumnActions relationDesc))

    dropColumnActions =
      concatMap
        (mkDropColumnActions tableDef)
        (PgCatalog.relationAttributes relationDesc)

    existingConstraints =
      Set.fromList
        . Maybe.mapMaybe pgConstraintMigrationKey
        . PgCatalog.relationConstraints
        $ relationDesc

    constraintsToKeep =
      Set.map (setDefaultSchemaNameOnConstraintKey currentNamespace)
        . Schema.tableConstraintKeys
        . Orville.tableConstraints
        $ tableDef

    addConstraintActions =
      concatMap
        (mkAddConstraintActions currentNamespace existingConstraints)
        (Schema.tableConstraintDefinitions $ Orville.tableConstraints tableDef)

    dropConstraintActions =
      concatMap
        (mkDropConstraintActions constraintsToKeep)
        (PgCatalog.relationConstraints relationDesc)

    systemIndexOids =
      Set.fromList
        . Maybe.mapMaybe (pgConstraintImpliedIndexOid . PgCatalog.constraintRecord)
        . PgCatalog.relationConstraints
        $ relationDesc

    isSystemIndex indexDesc =
      Set.member
        (PgCatalog.pgIndexPgClassOid $ PgCatalog.indexRecord indexDesc)
        systemIndexOids

    existingIndexes =
      Set.fromList
        . concatMap pgIndexMigrationKeys
        . filter (not . isSystemIndex)
        . PgCatalog.relationIndexes
        $ relationDesc

    indexesToKeep =
      Map.keysSet
        . Orville.tableIndexes
        $ tableDef

    addIndexSteps =
      concatMap
        (mkAddIndexSteps existingIndexes tableName)
        (Orville.tableIndexes tableDef)

    dropIndexSteps =
      concatMap
        (mkDropIndexSteps indexesToKeep systemIndexOids)
        (PgCatalog.relationIndexes relationDesc)

    tableName =
      Orville.tableName tableDef
  in
    mkAlterColumnSteps tableName (addAlterColumnActions <> dropColumnActions)
      <> mkConstraintSteps tableName (addConstraintActions <> dropConstraintActions)
      <> addIndexSteps
      <> dropIndexSteps

{- |
  Consolidates alter table actions (which should all be related to adding and
  dropping constraints) into migration steps based on their 'StepType'. Actions
  with the same 'StepType' will be performed togethir in a single @ALTER TABLE@
  statement.

@since 1.0.0.0
-}
mkConstraintSteps ::
  Expr.Qualified Expr.TableName ->
  [(StepType, Expr.AlterTableAction)] ->
  [MigrationStepWithType]
mkConstraintSteps tableName actions =
  let
    mkMapEntry ::
      (StepType, Expr.AlterTableAction) ->
      (StepType, NonEmpty Expr.AlterTableAction)
    mkMapEntry (keyType, action) =
      (keyType, (action :| []))

    addStep stepType actionExprs steps =
      mkMigrationStepWithType stepType (Expr.alterTableExpr tableName actionExprs) : steps
  in
    Map.foldrWithKey addStep []
      . Map.fromListWith (<>)
      . map mkMapEntry
      $ actions

{- |
  If there are any alter table actions for adding or removing columns, creates a migration
  step to perform them. Otherwise returns an empty list.

@since 1.0.0.0
-}
mkAlterColumnSteps ::
  Expr.Qualified Expr.TableName ->
  [Expr.AlterTableAction] ->
  [MigrationStepWithType]
mkAlterColumnSteps tableName actionExprs =
  case nonEmpty actionExprs of
    Nothing ->
      []
    Just nonEmptyActionExprs ->
      [mkMigrationStepWithType AddRemoveTablesAndColumns (Expr.alterTableExpr tableName nonEmptyActionExprs)]

{- |
  Builds 'Expr.AlterTableAction' expressions to bring the database schema in
  line with the given 'Orville.FieldDefinition', or none if no change is
  required.

@since 1.0.0.0
-}
mkAddAlterColumnActions ::
  PgCatalog.RelationDescription ->
  Orville.FieldDefinition nullability a ->
  [Expr.AlterTableAction]
mkAddAlterColumnActions relationDesc fieldDef =
  let
    pgAttributeName =
      String.fromString (Orville.fieldNameToString $ Orville.fieldName fieldDef)
  in
    case PgCatalog.lookupAttribute pgAttributeName relationDesc of
      Just attr
        | PgCatalog.isOrdinaryColumn attr ->
            let
              sqlType =
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
                Orville.fieldIsNotNullable fieldDef /= PgCatalog.pgAttributeIsNotNull attr

              nullabilityAction =
                if Orville.fieldIsNotNullable fieldDef
                  then Expr.setNotNull
                  else Expr.dropNotNull

              alterNullability = do
                guard nullabilityIsChanged
                [Expr.alterColumnNullability (Orville.fieldColumnName fieldDef) nullabilityAction]

              maybeExistingDefault =
                PgCatalog.lookupAttributeDefault attr relationDesc

              maybeDefaultExpr =
                Orville.defaultValueExpression
                  <$> Orville.fieldDefaultValue fieldDef

              (dropDefault, setDefault) =
                case (maybeExistingDefault, maybeDefaultExpr) of
                  (Nothing, Nothing) ->
                    (Nothing, Nothing)
                  (Just _, Nothing) ->
                    if Orville.sqlTypeDontDropImplicitDefaultDuringMigrate sqlType
                      then (Nothing, Nothing)
                      else
                        ( Just (Expr.alterColumnDropDefault columnName)
                        , Nothing
                        )
                  (Nothing, Just newDefault) ->
                    ( Nothing
                    , Just (Expr.alterColumnSetDefault columnName newDefault)
                    )
                  (Just oldDefault, Just newDefault) ->
                    let
                      oldDefaultExprBytes =
                        Enc.encodeUtf8
                          . PgCatalog.pgAttributeDefaultExpression
                          $ oldDefault

                      newDefaultExprBytes =
                        RawSql.toExampleBytes newDefault
                    in
                      if oldDefaultExprBytes == newDefaultExprBytes
                        then (Nothing, Nothing)
                        else
                          ( Just (Expr.alterColumnDropDefault columnName)
                          , Just (Expr.alterColumnSetDefault columnName newDefault)
                          )
            in
              Maybe.maybeToList dropDefault <> alterType <> Maybe.maybeToList setDefault <> alterNullability
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

{- |
  Builds 'Expr.AlterTableAction' expressions for the given attribute to make
  the database schema match the given 'Orville.TableDefinition'. This function
  is only responsible for handling cases where the attribute does not have a
  correspending 'Orville.FieldDefinition'. See 'mkAlterTableSteps' for those
  cases.

@since 1.0.0.0
-}
mkDropColumnActions ::
  Orville.TableDefinition key readEntity writeEntity ->
  PgCatalog.PgAttribute ->
  [Expr.AlterTableAction]
mkDropColumnActions tableDef attr = do
  let
    attrName =
      PgCatalog.attributeNameToString $ PgCatalog.pgAttributeName attr

  guard $ Set.member attrName (Orville.columnsToDrop tableDef)

  [Expr.dropColumn $ Expr.columnName attrName]

{- |
  Sets the schema name on a constraint to the given namespace when the
  constraint has no namespace explicitly given. This is important for Orville
  to discover whether a constraint from a table definition matches a constraint
  found to already exist in the database because constraints in the database
  always have schema names included with them.

@since 1.0.0.0
-}
setDefaultSchemaNameOnConstraintKey ::
  PgCatalog.NamespaceName ->
  Orville.ConstraintMigrationKey ->
  Orville.ConstraintMigrationKey
setDefaultSchemaNameOnConstraintKey currentNamespace constraintKey =
  case Orville.constraintKeyForeignTable constraintKey of
    Nothing ->
      constraintKey
    Just foreignTable ->
      case Orville.tableIdSchemaNameString foreignTable of
        Nothing ->
          constraintKey
            { Orville.constraintKeyForeignTable =
                Just $
                  Orville.setTableIdSchema
                    (PgCatalog.namespaceNameToString currentNamespace)
                    foreignTable
            }
        Just _ ->
          constraintKey

{- |
  Builds 'Expr.AlterTableAction' expressions to create the given table
  constraint if it does not exist.

@since 1.0.0.0
-}
mkAddConstraintActions ::
  PgCatalog.NamespaceName ->
  Set.Set Orville.ConstraintMigrationKey ->
  Orville.ConstraintDefinition ->
  [(StepType, Expr.AlterTableAction)]
mkAddConstraintActions currentNamespace existingConstraints constraintDef =
  let
    constraintKey =
      setDefaultSchemaNameOnConstraintKey currentNamespace $
        Orville.constraintMigrationKey constraintDef

    stepType =
      case Orville.constraintKeyType constraintKey of
        Orville.UniqueConstraint -> AddUniqueConstraints
        Orville.ForeignKeyConstraint -> AddForeignKeys
  in
    if Set.member constraintKey existingConstraints
      then []
      else [(stepType, Expr.addConstraint (Orville.constraintSqlExpr constraintDef))]

{- |
  Builds 'Expr.AlterTableAction' expressions to drop the given table
  constraint if it should not exist.

@since 1.0.0.0
-}
mkDropConstraintActions ::
  Set.Set Orville.ConstraintMigrationKey ->
  PgCatalog.ConstraintDescription ->
  [(StepType, Expr.AlterTableAction)]
mkDropConstraintActions constraintsToKeep constraint =
  case pgConstraintMigrationKey constraint of
    Nothing ->
      []
    Just constraintKey ->
      if Set.member constraintKey constraintsToKeep
        then []
        else
          let
            constraintName =
              Expr.constraintName
                . PgCatalog.constraintNameToString
                . PgCatalog.pgConstraintName
                . PgCatalog.constraintRecord
                $ constraint

            stepType =
              case Orville.constraintKeyType constraintKey of
                Orville.UniqueConstraint -> DropUniqueConstraints
                Orville.ForeignKeyConstraint -> DropForeignKeys
          in
            [(stepType, Expr.dropConstraint constraintName)]

{- |
  Builds the orville migration key for a description of an existing constraint
  so that it can be compared with constraints found in a table definition.
  Constraint keys built this way always have a schema name populated, so it's
  important to set the schema names for the constraints found in the table
  definition before comparing them. See 'setDefaultSchemaNameOnConstraintKey'.

  If the description is for a kind of constraint that Orville does not support,
  'Nothing' is returned.

@since 1.0.0.0
-}
pgConstraintMigrationKey ::
  PgCatalog.ConstraintDescription ->
  Maybe Orville.ConstraintMigrationKey
pgConstraintMigrationKey constraintDesc =
  let
    toOrvilleConstraintKeyType pgConType =
      case pgConType of
        PgCatalog.UniqueConstraint -> Just Orville.UniqueConstraint
        PgCatalog.ForeignKeyConstraint -> Just Orville.ForeignKeyConstraint
        _ -> Nothing

    constraint =
      PgCatalog.constraintRecord constraintDesc

    pgAttributeNamesToFieldNames =
      map (Orville.stringToFieldName . PgCatalog.attributeNameToString . PgCatalog.pgAttributeName)

    foreignRelationTableId :: PgCatalog.ForeignRelationDescription -> Orville.TableIdentifier
    foreignRelationTableId foreignRelationDesc =
      let
        relationName =
          PgCatalog.relationNameToString
            . PgCatalog.pgClassRelationName
            . PgCatalog.foreignRelationClass
            $ foreignRelationDesc

        namespaceName =
          PgCatalog.namespaceNameToString
            . PgCatalog.pgNamespaceName
            . PgCatalog.foreignRelationNamespace
            $ foreignRelationDesc
      in
        Orville.setTableIdSchema namespaceName $
          Orville.unqualifiedNameToTableId relationName
  in
    do
      keyType <- toOrvilleConstraintKeyType (PgCatalog.pgConstraintType constraint)
      pure $
        Orville.ConstraintMigrationKey
          { Orville.constraintKeyType = keyType
          , Orville.constraintKeyColumns =
              fmap
                pgAttributeNamesToFieldNames
                (PgCatalog.constraintKey constraintDesc)
          , Orville.constraintKeyForeignTable =
              fmap foreignRelationTableId (PgCatalog.constraintForeignRelation constraintDesc)
          , Orville.constraintKeyForeignColumns =
              fmap
                pgAttributeNamesToFieldNames
                (PgCatalog.constraintForeignKey constraintDesc)
          , Orville.constraintKeyForeignKeyOnUpdateAction =
              PgCatalog.pgConstraintForeignKeyOnUpdateType $ PgCatalog.constraintRecord constraintDesc
          , Orville.constraintKeyForeignKeyOnDeleteAction =
              PgCatalog.pgConstraintForeignKeyOnDeleteType $ PgCatalog.constraintRecord constraintDesc
          }

{- |
  Builds migration steps to create an index if it does not exist.

@since 1.0.0.0
-}
mkAddIndexSteps ::
  Set.Set IndexDefinition.IndexMigrationKey ->
  Expr.Qualified Expr.TableName ->
  Orville.IndexDefinition ->
  [MigrationStepWithType]
mkAddIndexSteps existingIndexes tableName indexDef =
  let
    indexKey =
      IndexDefinition.indexMigrationKey indexDef

    indexStep =
      case Orville.indexCreationStrategy indexDef of
        Orville.Transactional -> AddIndexesTransactionally
        Orville.Concurrent -> AddIndexesConcurrently
  in
    if Set.member indexKey existingIndexes
      then []
      else [mkMigrationStepWithType indexStep (Orville.indexCreateExpr indexDef tableName)]

{- |
  Builds migration steps to drop an index if it should not exist.

@since 1.0.0.0
-}
mkDropIndexSteps ::
  Set.Set IndexDefinition.IndexMigrationKey ->
  Set.Set LibPQ.Oid ->
  PgCatalog.IndexDescription ->
  [MigrationStepWithType]
mkDropIndexSteps indexesToKeep systemIndexOids indexDesc =
  case pgIndexMigrationKeys indexDesc of
    [] ->
      []
    indexKeys ->
      let
        pgClass =
          PgCatalog.indexPgClass indexDesc

        indexName =
          Expr.indexName
            . PgCatalog.relationNameToString
            . PgCatalog.pgClassRelationName
            $ pgClass

        indexOid =
          PgCatalog.pgClassOid pgClass
      in
        if any (flip Set.member indexesToKeep) indexKeys
          || Set.member indexOid systemIndexOids
          then []
          else [mkMigrationStepWithType DropIndexes (Expr.dropIndexExpr indexName)]

{- |
  Primary Key, Unique, and Exclusion constraints automatically create indexes
  that we don't want orville to consider for the purposes of migrations. This
  function checks the constraint type and returns the OID of the supporting
  index if the constraint is one of these types.

  Foreign key constraints also have a supporting index OID in @pg_catalog@, but
  this index is not automatically created due to the constraint, so we don't
  return the index's OID for that case.

@since 1.0.0.0
-}
pgConstraintImpliedIndexOid :: PgCatalog.PgConstraint -> Maybe LibPQ.Oid
pgConstraintImpliedIndexOid pgConstraint =
  case PgCatalog.pgConstraintType pgConstraint of
    PgCatalog.PrimaryKeyConstraint ->
      Just $ PgCatalog.pgConstraintIndexOid pgConstraint
    PgCatalog.UniqueConstraint ->
      Just $ PgCatalog.pgConstraintIndexOid pgConstraint
    PgCatalog.ExclusionConstraint ->
      Just $ PgCatalog.pgConstraintIndexOid pgConstraint
    PgCatalog.CheckConstraint ->
      Nothing
    PgCatalog.ForeignKeyConstraint ->
      Nothing
    PgCatalog.ConstraintTrigger ->
      Nothing

{- |
  Builds the orville migration keys given a description of an existing index
  so that it can be compared with indexs found in a table definition.

  If the description includes expressions as members of the index rather than
  simple attributes, 'Nothing' is returned.

@since 1.0.0.0
-}
pgIndexMigrationKeys ::
  PgCatalog.IndexDescription ->
  [IndexDefinition.IndexMigrationKey]
pgIndexMigrationKeys indexDesc =
  let
    mkNamedIndexKey =
      IndexDefinition.NamedIndexKey
        . PgCatalog.relationNameToString
        . PgCatalog.pgClassRelationName
        . PgCatalog.indexPgClass
        $ indexDesc
    mkAttributeBasedIndexKey =
      case pgAttributeBasedIndexMigrationKey indexDesc of
        Just standardKey ->
          [IndexDefinition.AttributeBasedIndexKey standardKey]
        Nothing ->
          []
  in
    [mkNamedIndexKey] ++ mkAttributeBasedIndexKey

pgAttributeBasedIndexMigrationKey ::
  PgCatalog.IndexDescription ->
  Maybe IndexDefinition.AttributeBasedIndexMigrationKey
pgAttributeBasedIndexMigrationKey indexDesc = do
  let
    indexMemberToFieldName member =
      case member of
        PgCatalog.IndexAttribute attr ->
          Just (Orville.stringToFieldName . PgCatalog.attributeNameToString . PgCatalog.pgAttributeName $ attr)
        PgCatalog.IndexExpression ->
          Nothing

    uniqueness =
      if PgCatalog.pgIndexIsUnique (PgCatalog.indexRecord indexDesc)
        then Orville.UniqueIndex
        else Orville.NonUniqueIndex

  fieldNames <- traverse indexMemberToFieldName (PgCatalog.indexMembers indexDesc)
  pure $
    IndexDefinition.AttributeBasedIndexMigrationKey
      { IndexDefinition.indexKeyUniqueness = uniqueness
      , IndexDefinition.indexKeyColumns = fieldNames
      }

schemaItemPgCatalogRelation ::
  PgCatalog.NamespaceName ->
  SchemaItem ->
  (PgCatalog.NamespaceName, PgCatalog.RelationName)
schemaItemPgCatalogRelation currentNamespace item =
  case item of
    SchemaTable tableDef ->
      tableIdToPgCatalogNames currentNamespace (Orville.tableIdentifier tableDef)
    SchemaDropTable tableId ->
      tableIdToPgCatalogNames currentNamespace tableId
    SchemaSequence sequenceDef ->
      sequenceIdToPgCatalogNames currentNamespace (Orville.sequenceIdentifier sequenceDef)
    SchemaDropSequence sequenceId ->
      sequenceIdToPgCatalogNames currentNamespace sequenceId

tableIdToPgCatalogNames ::
  PgCatalog.NamespaceName ->
  Orville.TableIdentifier ->
  (PgCatalog.NamespaceName, PgCatalog.RelationName)
tableIdToPgCatalogNames currentNamespace tableId =
  let
    actualNamespace =
      maybe currentNamespace String.fromString
        . Orville.tableIdSchemaNameString
        $ tableId

    relationName =
      String.fromString
        . Orville.tableIdUnqualifiedNameString
        $ tableId
  in
    (actualNamespace, relationName)

mkAlterSequenceSteps ::
  Orville.SequenceDefinition ->
  PgCatalog.PgSequence ->
  [MigrationStepWithType]
mkAlterSequenceSteps sequenceDef pgSequence =
  let
    ifChanged ::
      Eq a =>
      (a -> expr) ->
      (PgCatalog.PgSequence -> a) ->
      (Orville.SequenceDefinition -> a) ->
      Maybe expr
    ifChanged mkChange getOld getNew =
      if getOld pgSequence == getNew sequenceDef
        then Nothing
        else Just . mkChange . getNew $ sequenceDef

    mbIncrementByExpr =
      ifChanged Expr.incrementBy PgCatalog.pgSequenceIncrement Orville.sequenceIncrement

    mbMinValueExpr =
      ifChanged Expr.minValue PgCatalog.pgSequenceMin Orville.sequenceMinValue

    mbMaxValueExpr =
      ifChanged Expr.maxValue PgCatalog.pgSequenceMax Orville.sequenceMaxValue

    mbStartWithExpr =
      ifChanged Expr.startWith PgCatalog.pgSequenceStart Orville.sequenceStart

    mbCacheExpr =
      ifChanged Expr.cache PgCatalog.pgSequenceCache Orville.sequenceCache

    mbCycleExpr =
      ifChanged Expr.cycleIfTrue PgCatalog.pgSequenceCycle Orville.sequenceCycle

    applyChange :: (Bool, Maybe a -> b) -> Maybe a -> (Bool, b)
    applyChange (changed, exprF) mbArg =
      (changed || Maybe.isJust mbArg, exprF mbArg)

    (anyChanges, migrateSequenceExpr) =
      (False, Expr.alterSequenceExpr (Orville.sequenceName sequenceDef))
        `applyChange` mbIncrementByExpr
        `applyChange` mbMinValueExpr
        `applyChange` mbMaxValueExpr
        `applyChange` mbStartWithExpr
        `applyChange` mbCacheExpr
        `applyChange` mbCycleExpr
  in
    if anyChanges
      then [mkMigrationStepWithType AddRemoveTablesAndColumns migrateSequenceExpr]
      else []

sequenceIdToPgCatalogNames ::
  PgCatalog.NamespaceName ->
  Orville.SequenceIdentifier ->
  (PgCatalog.NamespaceName, PgCatalog.RelationName)
sequenceIdToPgCatalogNames currentNamespace sequenceId =
  let
    actualNamespace =
      maybe currentNamespace String.fromString
        . Orville.sequenceIdSchemaNameString
        $ sequenceId

    relationName =
      String.fromString
        . Orville.sequenceIdUnqualifiedNameString
        $ sequenceId
  in
    (actualNamespace, relationName)

currentNamespaceQuery :: Expr.QueryExpr
currentNamespaceQuery =
  Expr.queryExpr
    (Expr.selectClause (Expr.selectExpr Nothing))
    ( Expr.selectDerivedColumns
        [ Expr.deriveColumnAs
            -- current_schema is a special reserved word in postgresql. If you
            -- put it in quotes it tries to treat it as a regular column name,
            -- which then can't be found as a column in the query.
            (RawSql.unsafeSqlExpression "current_schema")
            (Orville.fieldColumnName PgCatalog.namespaceNameField)
        ]
    )
    Nothing

findCurrentNamespace :: Orville.MonadOrville m => m PgCatalog.NamespaceName
findCurrentNamespace = do
  results <-
    Orville.executeAndDecode
      Orville.SelectQuery
      currentNamespaceQuery
      (Orville.annotateSqlMarshallerEmptyAnnotation $ Orville.marshallField id PgCatalog.namespaceNameField)

  liftIO $
    case results of
      [schemaAndCatalog] ->
        pure schemaAndCatalog
      [] ->
        throwIO $ UnableToDiscoverCurrentSchema "No results returned by current_schema query"
      _ ->
        throwIO $ UnableToDiscoverCurrentSchema "Multiple results returned by current_schema query"
