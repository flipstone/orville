{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.AutoMigration
  ( autoMigrateSchema,
    generateMigrationSteps,
    executeMigrationSteps,
    SchemaItem,
    schemaTable,
    schemaItemSummary,
    MigrationStep,
    MigrationDataError,
  )
where

import Control.Exception.Safe (Exception, throwIO)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Database.PostgreSQL.LibPQ as LibPQ

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
  Retuns a one-line string describe the 'SchemaItem', suitable for a human to
  identify it in a list of output.

  For example, a 'SchemaItem' constructed via 'schemaTable' gives @Table <table
  name>@.
-}
schemaItemSummary :: SchemaItem -> String
schemaItemSummary item =
  case item of
    SchemaTable tableDef ->
      "Table " <> Orville.tableIdToString (Orville.tableIdentifier tableDef)

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
  This type is used internally by Orville to order the migration steps after
  they have been created. It is not exposed outside this module.
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

{- |
  Indicates the kind of operation being performed by a 'MigrationStep' so
  that the steps can be ordered in a sequence that is guaranteed to succeed.
  The order of the constructors below indicates the order in which steps will
  be run.
-}
data StepType
  = DropForeignKeys
  | DropUniqueConstraints
  | DropIndexes
  | AddRemoveTablesAndColumns
  | AddIndexes
  | AddUniqueConstraints
  | AddForeignKeys
  deriving (Eq, Ord)

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

  pure
    . map migrationStep
    . List.sortOn migrationStepType
    . concatMap (calculateMigrationSteps currentNamespace dbDesc)
    $ schemaItems

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
  [MigrationStepWithType]
calculateMigrationSteps currentNamespace dbDesc schemaItem =
  case schemaItem of
    SchemaTable tableDef ->
      let schemaName =
            tableDefinitionActualNamespaceName currentNamespace tableDef

          tableName =
            String.fromString
              . Orville.tableIdUnqualifiedNameString
              . Orville.tableIdentifier
              $ tableDef
       in case PgCatalog.lookupRelation (schemaName, tableName) dbDesc of
            Nothing ->
              mkCreateTableSteps currentNamespace tableDef
            Just relationDesc ->
              mkAlterTableSteps currentNamespace relationDesc tableDef

{- |
  Builds 'MigrationStep's that will perform table creation. This function
  assumes the table does not exist. The migration step it produces will fail if
  the table already exists in its schema. Multiple steps may be required to
  create the table if foreign keys exist to that reference other tables, which
  may not have been created yet.
-}
mkCreateTableSteps ::
  PgCatalog.NamespaceName ->
  Orville.TableDefinition key writeEntity readEntity ->
  [MigrationStepWithType]
mkCreateTableSteps currentNamespace tableDef =
  let tableName =
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
          (Orville.tableConstraints tableDef)

      addIndexSteps =
        concatMap
          (mkAddIndexSteps Set.empty tableName)
          (Orville.tableIndexes tableDef)
   in mkMigrationStepWithType AddRemoveTablesAndColumns createTableExpr :
      mkConstraintSteps tableName addConstraintActions
        <> addIndexSteps

{- |
  Builds migration steps that are required to create or alter the table's
  schema to make it match the given table definition.

  This function uses the given relation description to determine what
  alterations need to be performed. If there is nothing to do, an empty list
  will be returned.
-}
mkAlterTableSteps ::
  PgCatalog.NamespaceName ->
  PgCatalog.RelationDescription ->
  Orville.TableDefinition key writeEntity readEntity ->
  [MigrationStepWithType]
mkAlterTableSteps currentNamespace relationDesc tableDef =
  let addAlterColumnActions =
        concat $
          Orville.foldMarshallerFields
            (Orville.tableMarshaller tableDef)
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
          . Map.keysSet
          . Orville.tableConstraints
          $ tableDef

      addConstraintActions =
        concatMap
          (mkAddConstraintActions currentNamespace existingConstraints)
          (Orville.tableConstraints tableDef)

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
          . Maybe.mapMaybe pgIndexMigrationKey
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
   in mkAlterColumnSteps tableName (addAlterColumnActions <> dropColumnActions)
        <> mkConstraintSteps tableName (addConstraintActions <> dropConstraintActions)
        <> addIndexSteps
        <> dropIndexSteps

{- |
  Consolidates alter table actions (which should all be related to adding and
  dropping constraints) into migration steps based on their 'StepType'. Actions
  with the same 'StepType' will be performed togethir in a single @ALTER TABLE@
  statement.
-}
mkConstraintSteps ::
  Expr.QualifiedTableName ->
  [(StepType, Expr.AlterTableAction)] ->
  [MigrationStepWithType]
mkConstraintSteps tableName actions =
  let mkMapEntry ::
        (StepType, Expr.AlterTableAction) ->
        (StepType, NonEmpty Expr.AlterTableAction)
      mkMapEntry (keyType, action) =
        (keyType, (action :| []))

      addStep stepType actionExprs steps =
        mkMigrationStepWithType stepType (Expr.alterTableExpr tableName actionExprs) : steps
   in Map.foldrWithKey addStep []
        . Map.fromListWith (<>)
        . map mkMapEntry
        $ actions

{- |
  If there are any alter table actions for adding or removing columns, creates a migration
  step to perform them. Otherwise returns an empty list.
-}
mkAlterColumnSteps ::
  Expr.QualifiedTableName ->
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
-}
mkAddAlterColumnActions ::
  PgCatalog.RelationDescription ->
  Orville.FieldDefinition nullability a ->
  [Expr.AlterTableAction]
mkAddAlterColumnActions relationDesc fieldDef =
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

{- |
  Builds 'Expr.AlterTableAction' expressions for the given attribute to make
  the database schema match the given 'Orville.TableDefinition'. This function
  is only responsible for handling cases where the attribute does not have a
  correspending 'Orville.FieldDefinition'. See 'mkFieldActions' for those
  cases.
-}
mkDropColumnActions ::
  Orville.TableDefinition key readEntity writeEntity ->
  PgCatalog.PgAttribute ->
  [Expr.AlterTableAction]
mkDropColumnActions tableDef attr = do
  let attrName =
        PgCatalog.attributeNameToString $ PgCatalog.pgAttributeName attr

  guard $ Set.member attrName (Orville.columnsToDrop tableDef)

  [Expr.dropColumn $ Expr.columnName attrName]

{- |
  Sets the schema name on a constraint to the given namespace when the
  constraint has no namespace explicitly given. This is important for Orville
  to discover whether a constraint from a table definition matches a constraint
  found to already exist in the database because constraints in the database
  always have schema names included with them.
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
-}
mkAddConstraintActions ::
  PgCatalog.NamespaceName ->
  Set.Set Orville.ConstraintMigrationKey ->
  Orville.ConstraintDefinition ->
  [(StepType, Expr.AlterTableAction)]
mkAddConstraintActions currentNamespace existingConstraints constraintDef =
  let constraintKey =
        setDefaultSchemaNameOnConstraintKey currentNamespace $
          Orville.constraintMigrationKey constraintDef

      stepType =
        case Orville.constraintKeyType constraintKey of
          Orville.UniqueConstraint -> AddUniqueConstraints
          Orville.ForeignKeyConstraint -> AddForeignKeys
   in if Set.member constraintKey existingConstraints
        then []
        else [(stepType, Expr.addConstraint (Orville.constraintSqlExpr constraintDef))]

{- |
  Builds 'Expr.AlterTableAction' expressions to drop the given table
  constraint if it should not exist.
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
          let constraintName =
                Expr.constraintName
                  . PgCatalog.constraintNameToString
                  . PgCatalog.pgConstraintName
                  . PgCatalog.constraintRecord
                  $ constraint

              stepType =
                case Orville.constraintKeyType constraintKey of
                  Orville.UniqueConstraint -> DropUniqueConstraints
                  Orville.ForeignKeyConstraint -> DropForeignKeys
           in [(stepType, Expr.dropConstraint constraintName)]

{- |
  Builds the orville migration key for a description of an existing constraint
  so that it can be compared with constraints found in a table definition.
  Constraint keys built this way always have a schema name populated, so it's
  important to set the schema names for the constraints found in the table
  definition before comparing them. See 'setDefaultSchemaNameOnConstraintKey'.

  If the description is for a kind of constraint that Orville does not support,
  'Nothing' is returned.
-}
pgConstraintMigrationKey ::
  PgCatalog.ConstraintDescription ->
  Maybe Orville.ConstraintMigrationKey
pgConstraintMigrationKey constraintDesc =
  let toOrvilleConstraintKeyType pgConType =
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
        let relationName =
              PgCatalog.relationNameToString
                . PgCatalog.pgClassRelationName
                . PgCatalog.foreignRelationClass
                $ foreignRelationDesc

            namespaceName =
              PgCatalog.namespaceNameToString
                . PgCatalog.pgNamespaceName
                . PgCatalog.foreignRelationNamespace
                $ foreignRelationDesc
         in Orville.setTableIdSchema namespaceName $
              Orville.unqualifiedNameToTableId relationName
   in do
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
            }

{- |
  Builds migration steps to create an index if it does not exist.
-}
mkAddIndexSteps ::
  Set.Set Orville.IndexMigrationKey ->
  Expr.QualifiedTableName ->
  Orville.IndexDefinition ->
  [MigrationStepWithType]
mkAddIndexSteps existingIndexes tableName indexDef =
  let indexKey =
        Orville.indexMigrationKey indexDef
   in if Set.member indexKey existingIndexes
        then []
        else [mkMigrationStepWithType AddIndexes (Orville.indexCreateExpr indexDef tableName)]

{- |
  Builds migration steps to create an index if it does not exist.
-}
mkDropIndexSteps ::
  Set.Set Orville.IndexMigrationKey ->
  Set.Set LibPQ.Oid ->
  PgCatalog.IndexDescription ->
  [MigrationStepWithType]
mkDropIndexSteps indexesToKeep systemIndexOids indexDesc =
  case pgIndexMigrationKey indexDesc of
    Nothing ->
      []
    Just indexKey ->
      let pgClass =
            PgCatalog.indexPgClass indexDesc

          indexName =
            Expr.indexName
              . PgCatalog.relationNameToString
              . PgCatalog.pgClassRelationName
              $ pgClass

          indexOid =
            PgCatalog.pgClassOid pgClass
       in if Set.member indexKey indexesToKeep
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
  Builds the orville migration key for a description of an existing index
  so that it can be compared with indexs found in a table definition.

  If the description is for includes expressions as members of the index
  rather than simple attributes, 'Nothing' is returned.
-}
pgIndexMigrationKey ::
  PgCatalog.IndexDescription ->
  Maybe Orville.IndexMigrationKey
pgIndexMigrationKey indexDesc = do
  let indexMemberToFieldName member =
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
    Orville.IndexMigrationKey
      { Orville.indexKeyUniqueness = uniqueness
      , Orville.indexKeyColumns = fieldNames
      }

schemaItemPgCatalogRelation ::
  PgCatalog.NamespaceName ->
  SchemaItem ->
  (PgCatalog.NamespaceName, PgCatalog.RelationName)
schemaItemPgCatalogRelation currentNamespace item =
  case item of
    SchemaTable tableDef ->
      ( tableDefinitionActualNamespaceName currentNamespace tableDef
      , String.fromString . Orville.tableIdUnqualifiedNameString . Orville.tableIdentifier $ tableDef
      )

tableDefinitionActualNamespaceName ::
  PgCatalog.NamespaceName ->
  Orville.TableDefinition key writeEntity readEntity ->
  PgCatalog.NamespaceName
tableDefinitionActualNamespaceName currentNamespace =
  maybe currentNamespace String.fromString
    . Orville.tableIdSchemaNameString
    . Orville.tableIdentifier

currentNamespaceQuery :: Expr.QueryExpr
currentNamespaceQuery =
  Expr.queryExpr
    (Expr.selectClause (Expr.selectExpr Nothing))
    ( Expr.selectDerivedColumns
        [ Expr.deriveColumnAs
            -- current_schema is a special reserved word in postgresql. If you
            -- put it in quotes it tries to treat it as a regular column name,
            -- which then can't be found as a column in the query.
            (Expr.fromIdentifier (Expr.unquotedIdentifier "current_schema"))
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
