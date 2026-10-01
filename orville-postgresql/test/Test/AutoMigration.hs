{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.AutoMigration
  ( autoMigrationTests
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as Fold
import qualified Data.Function as Function
import Data.Int (Int32, Int64)
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema as Schema
import qualified Orville.PostgreSQL.Schema.TableDefinition as TableDefinition
import qualified Test.Entities.Foo as Foo
import Test.Orphans ()
import qualified Test.PgAssert as PgAssert
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

autoMigrationTests :: Orville.ConnectionPool -> Tasty.TestTree
autoMigrationTests pool =
  Tasty.testGroup
    "AutoMigration"
    [ TastyHH.testProperty
        "Alters an existing column to add IDENTITY"
        (prop_altersColumnAddIdentity pool)
    , TastyHH.testProperty
        "Raises an error when the migration lock is hold"
        (prop_raisesErrorIfMigrationLockIsLocked pool)
    , TastyHH.testProperty
        "Releases the migration lock on error"
        (prop_releasesMigrationLockOnError pool)
    , TastyHH.testProperty
        "Creates missing tables"
        (prop_createsMissingTables pool)
    , TastyHH.testProperty
        "Drops requested tables"
        (prop_dropsRequestedTables pool)
    , TastyHH.testProperty
        "Adds and removes columns"
        (prop_addsAndRemovesColumns pool)
    , TastyHH.testProperty
        "An error is raised trying to add a column that conflicts with a system name"
        (prop_columnsWithSystemNameConflictsRaiseError pool)
    , TastyHH.testProperty
        "Alters data type on existing column"
        (prop_altersColumnDataType pool)
    , TastyHH.testProperty
        "Alters default value on existing column (text/numeric)"
        (prop_altersColumnDefaultValue_TextNumeric pool)
    , TastyHH.testProperty
        "Alters default value on existing column (integral boundaries)"
        (prop_altersColumnDefaultValue_IntegralBoundaries pool)
    , TastyHH.testProperty
        "Alters default value on existing column (boolean)"
        (prop_altersColumnDefaultValue_Bool pool)
    , TastyHH.testProperty
        "Alters default value on existing column (timelike)"
        (prop_altersColumnDefaultValue_Timelike pool)
    , TastyHH.testProperty
        "Respects implicit default on serial fields"
        (prop_respectsImplicitDefaultOnSerialFields pool)
    , TastyHH.testProperty
        "Adds and removes check constraints"
        (prop_addAndRemovesCheckConstraints pool)
    , TastyHH.testProperty
        "Adds a named unique constraint"
        (prop_addNamedUniqueConstraint pool)
    , TastyHH.testProperty
        "Adds and removes unique constraints"
        (prop_addAndRemovesUniqueConstraints pool)
    , TastyHH.testProperty
        "Adds and removes foreign key constraints"
        (prop_addAndRemovesForeignKeyConstraints pool)
    , TastyHH.testProperty
        "Creates missing sequences"
        (prop_createsMissingSequences pool)
    , TastyHH.testProperty
        "Drops requested sequences"
        (prop_dropsRequestedSequences pool)
    , TastyHH.testProperty
        "Alters modified sequences"
        (prop_altersModifiedSequences pool)
    , TastyHH.testProperty
        "Adds and removes named indexes"
        (prop_addsAndRemovesMixedIndexes pool)
    , TastyHH.testProperty
        "An arbitrary list of schema items can be created from scratch"
        (prop_arbitrarySchemaInitialMigration pool)
    , TastyHH.testProperty
        "Creates missing functions"
        (prop_createsMissingFunctions pool)
    , TastyHH.testProperty
        "Recreates functions with altered source code"
        (prop_recreatesAlteredFunctions pool)
    , TastyHH.testProperty
        "Drops requested functions"
        (prop_dropsRequestedFunctions pool)
    , TastyHH.testProperty
        "Creates missing triggers"
        (prop_createsMissingTriggers pool)
    , TastyHH.testProperty
        "Drops unrequested triggers"
        (prop_dropsUnrequestedTriggers pool)
    , TastyHH.testProperty
        "Loads missing extensions"
        (prop_loadsMissingExtensions pool)
    , TastyHH.testProperty
        "Unloads present extensions"
        (prop_unloadsPresentExtensions pool)
    , TastyHH.testProperty
        "Adds a comment to a table"
        (prop_addsTableComment pool)
    , TastyHH.testProperty
        "Removes a comment from a table"
        (prop_removesTableComment pool)
    , TastyHH.testProperty
        "Modifies the comment on a table"
        (prop_modifiesTableComment pool)
    , TastyHH.testProperty
        "Adds column comments when creating table"
        (prop_addsColumnCommmentsOnCreateTable pool)
    , TastyHH.testProperty
        "Modifies column comments"
        (prop_modifiesColumnComments pool)
    , TastyHH.testProperty
        "Creates missing policies"
        (prop_createsMissingPolicies pool)
    , TastyHH.testProperty
        "Drops requested policies"
        (prop_dropsRequestedPolicies pool)
    , TastyHH.testProperty
        "Recreates modified policies"
        (prop_recreatesModifiedPolicies pool)
    , TastyHH.testProperty
        "Creates restrictive policies"
        (prop_createsRestrictivePolicies pool)
    , TastyHH.testProperty
        "Recreates policies whose permission changed"
        (prop_recreatesPoliciesWithChangedPermission pool)
    , TastyHH.testProperty
        "Recreates policies whose command changed"
        (prop_recreatesPoliciesWithChangedCommand pool)
    , TastyHH.testProperty
        "Recreates policies whose USING expression is removed"
        (prop_recreatesPoliciesWithRemovedExpressions pool)
    , TastyHH.testProperty
        "Recreates a changed policy so a column it referenced can be dropped"
        (prop_recreatesPoliciesAcrossColumnChanges pool)
    , TastyHH.testProperty
        "An error is raised for policy definitions PostgreSQL would reject"
        (prop_invalidPolicyDefinitionsRaiseError pool)
    , TastyHH.testProperty
        "Normalizes PUBLIC role targets the way PostgreSQL does"
        (prop_normalizesPublicRoleTargets pool)
    , TastyHH.testProperty
        "Manages policies on tables with an explicit schema"
        (prop_managesPoliciesOnSchemaQualifiedTables pool)
    , TastyHH.testProperty
        "Enables row level security even when only dropping policies"
        (prop_enablesRowLevelSecurityWithoutPolicyCreation pool)
    , TastyHH.testProperty
        "Disables row level security when the definition does not enable it"
        (prop_disablesRowLevelSecurityWhenNotRequested pool)
    , TastyHH.testProperty
        "Distinguishes policies whose string literals differ only by case"
        (prop_altersPoliciesWhoseLiteralsDifferOnlyByCase pool)
    , TastyHH.testProperty
        "Creates and recreates policies with role targets"
        (prop_managesPolicyRoleTargets pool)
    , TastyHH.testProperty
        "Creates and recreates policies with WITH CHECK expressions"
        (prop_managesPoliciesWithCheckExprs pool)
    , TastyHH.testProperty
        "Creates policies applying to multiple roles"
        (prop_managesPoliciesWithMultipleRoles pool)
    , TastyHH.testProperty
        "Parses policy role names containing special characters"
        (prop_managesPoliciesWithQuotedRoleNames pool)
    , TastyHH.testProperty
        "Creates, recreates and drops multiple policies in one plan"
        (prop_managesMultiplePoliciesOnOneTable pool)
    , TastyHH.testProperty
        "An error is raised when a policy is both defined and marked for dropping"
        (prop_conflictingPolicyDefinitionsRaiseError pool)
    , TastyHH.testProperty
        "Distinguishes quoted identifiers that differ only by case"
        (prop_altersPoliciesWithQuotedIdentifiers pool)
    , TastyHH.testProperty
        "Distinguishes literals containing escaped quotes by case"
        (prop_altersPoliciesWithEscapedQuoteLiterals pool)
    , TastyHH.testProperty
        "addTablePolicies accumulates policies across calls, replacing by name"
        prop_addTablePoliciesAccumulates
    ]

prop_raisesErrorIfMigrationLockIsLocked :: Orville.ConnectionPool -> HH.Property
prop_raisesErrorIfMigrationLockIsLocked pool =
  Property.singletonProperty $ do
    let
      testLockOptions =
        AutoMigration.defaultLockOptions
          { AutoMigration.maxLockAttempts = 3
          , AutoMigration.delayBetweenLockAttemptsMicros = 1000
          , AutoMigration.lockDelayVariationMicros = 0
          }

    errOrSuccess <-
      HH.evalIO $
        Orville.runOrville pool $
          AutoMigration.withMigrationLock testLockOptions $
            MIO.liftIO $
              ExSafe.try $
                Orville.runOrville pool $
                  AutoMigration.withMigrationLock
                    testLockOptions
                    (pure ())

    case errOrSuccess of
      Left (_err :: AutoMigration.MigrationLockError) -> pure ()
      Right () -> do
        HH.annotate "Expected MigrationLockError error to be thrown, but it was not"
        HH.failure

prop_releasesMigrationLockOnError :: Orville.ConnectionPool -> HH.Property
prop_releasesMigrationLockOnError pool =
  Property.singletonProperty $ do
    HH.evalIO $
      Orville.runOrville pool $
        -- Acquire a connection before running a second orville context to
        -- ensure that each context will get a separate connection from the
        -- pool. Both connections must be open simultaneously for this test
        -- to be valid since the lock we are testing is held at a session
        -- level.
        Orville.withConnection_ $ do
          MIO.liftIO $
            Orville.runOrville pool $
              ExSafe.handle (\SimulatedError -> pure ()) $
                AutoMigration.withMigrationLock
                  AutoMigration.defaultLockOptions
                  (ExSafe.throwM SimulatedError)

          -- If the lock is not released by the previous 'withMigrationLock'
          -- this second call will fail to acquire the lock and the test will
          -- fail
          AutoMigration.withMigrationLock
            AutoMigration.defaultLockOptions
            (pure ())

data SimulatedError = SimulatedError
  deriving (Show)

instance ExSafe.Exception SimulatedError

prop_createsMissingTables :: Orville.ConnectionPool -> HH.Property
prop_createsMissingTables pool =
  Property.singletonProperty $ do
    let
      fooTableId =
        Orville.tableIdentifier Foo.table

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql Foo.table
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable Foo.table]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable Foo.table]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    _ <-
      PgAssert.assertTableExists
        pool
        (Orville.tableIdUnqualifiedNameString fooTableId)
    migrationPlanStepStrings secondTimePlan === []

prop_dropsRequestedTables :: Orville.ConnectionPool -> HH.Property
prop_dropsRequestedTables pool =
  Property.singletonProperty $ do
    let
      fooTableId =
        Orville.tableIdentifier Foo.table

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql Foo.table
          Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateTableExpr Foo.table
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropTable fooTableId]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropTable fooTableId]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertTableDoesNotExist pool (Orville.tableIdUnqualifiedNameString fooTableId)
    migrationPlanStepStrings secondTimePlan === []

prop_addsTableComment :: Orville.ConnectionPool -> HH.Property
prop_addsTableComment pool =
  Property.singletonProperty $ do
    let
      comment = String.fromString "This is a comment"

      fooTableWithComment = Orville.setTableComment comment Foo.table

      fooTableId =
        Orville.tableIdentifier fooTableWithComment

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql fooTableWithComment
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable fooTableWithComment]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable fooTableWithComment]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 2
    PgAssert.assertTableHasComment pool (Orville.tableIdUnqualifiedNameString fooTableId) comment
    migrationPlanStepStrings secondTimePlan === []

prop_removesTableComment :: Orville.ConnectionPool -> HH.Property
prop_removesTableComment pool =
  Property.singletonProperty $ do
    let
      comment = String.fromString "This is a comment"

      fooTableId =
        Orville.tableIdentifier Foo.table

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql Foo.table
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr Foo.table
          Orville.executeVoid Orville.DDLQuery $ Expr.commentTableExpr (Orville.tableName Foo.table) (Just $ Expr.commentText comment)
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable Foo.table]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable Foo.table]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertTableDoesNotHaveComment pool (Orville.tableIdUnqualifiedNameString fooTableId)
    migrationPlanStepStrings secondTimePlan === []

prop_modifiesTableComment :: Orville.ConnectionPool -> HH.Property
prop_modifiesTableComment pool =
  Property.singletonProperty $ do
    let
      oldComment = String.fromString "This is a comment"
      newComment = String.fromString "This is a new comment"

      fooTableId =
        Orville.tableIdentifier Foo.table

      fooTableWithNewComment = Orville.setTableComment newComment Foo.table

    HH.evalIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql Foo.table
        Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr Foo.table
        Orville.executeVoid Orville.DDLQuery $ Expr.commentTableExpr (Orville.tableName Foo.table) (Just $ Expr.commentText oldComment)

    PgAssert.assertTableHasComment pool (Orville.tableIdUnqualifiedNameString fooTableId) oldComment

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable fooTableWithNewComment]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable fooTableWithNewComment]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertTableHasComment pool (Orville.tableIdUnqualifiedNameString fooTableId) newComment
    migrationPlanStepStrings secondTimePlan === []

prop_addsColumnCommmentsOnCreateTable :: Orville.ConnectionPool -> HH.Property
prop_addsColumnCommmentsOnCreateTable pool =
  Property.singletonProperty $ do
    let
      columnsAndComments = [("foo", Just "foo comment"), ("bar", Nothing), ("baz", Just "baz comment")]
      tableName = "migration_test"
      tableDef = mkIntListTableWithComments tableName columnsAndComments

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql tableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable tableDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable tableDef]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 3
    PgAssert.assertTableColumnsHaveOrDoNotHaveComments pool tableName columnsAndComments
    migrationPlanStepStrings secondTimePlan === []

prop_modifiesColumnComments :: Orville.ConnectionPool -> HH.Property
prop_modifiesColumnComments pool =
  Property.singletonProperty $ do
    let
      initialColumnsAndComments = [("foo", Just "foo comment"), ("bar", Nothing), ("baz", Nothing)]
      finalColumnsAndComments = [("foo", Nothing), ("bar", Just "bar comment"), ("baz", Just "baz comment")]
      tableName = "migration_test"
      initialTableDef = mkIntListTableWithComments tableName initialColumnsAndComments
      finalTableDef = mkIntListTableWithComments tableName finalColumnsAndComments

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql initialTableDef
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr initialTableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable initialTableDef]

    MIO.liftIO . Orville.runOrville pool $
      AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
    PgAssert.assertTableColumnsHaveOrDoNotHaveComments pool tableName initialColumnsAndComments

    secondTimePlan <-
      HH.evalIO . Orville.runOrville pool $
        AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable finalTableDef]

    MIO.liftIO . Orville.runOrville pool $
      AutoMigration.executeMigrationPlan AutoMigration.defaultOptions secondTimePlan
    PgAssert.assertTableColumnsHaveOrDoNotHaveComments pool tableName finalColumnsAndComments

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    length (AutoMigration.migrationPlanSteps secondTimePlan) === 3

prop_addsAndRemovesColumns :: Orville.ConnectionPool -> HH.Property
prop_addsAndRemovesColumns pool =
  HH.property $ do
    let
      genColumnList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    originalColumns <- HH.forAll genColumnList
    newColumns <- HH.forAll genColumnList

    let
      columnsToDrop =
        originalColumns \\ newColumns

      originalTableDef =
        mkIntListTable "migration_test" originalColumns

      newTableDef =
        Orville.dropColumns columnsToDrop $
          mkIntListTable "migration_test" newColumns

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    PgAssert.assertColumnNamesEqual tableDesc newColumns

prop_columnsWithSystemNameConflictsRaiseError :: Orville.ConnectionPool -> HH.Property
prop_columnsWithSystemNameConflictsRaiseError pool =
  Property.singletonProperty $ do
    let
      tableWithSystemAttributeNames =
        Orville.mkTableDefinitionWithoutKey
          "table_with_system_attribute_names"
          (Orville.marshallField id (Orville.unboundedTextField "tableoid"))

    result <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql tableWithSystemAttributeNames

          -- Create the table with no columns first to ensure we go down the
          -- "add column" path
          Orville.executeVoid Orville.DDLQuery $
            Expr.createTableExpr
              (Orville.tableName tableWithSystemAttributeNames)
              []
              Nothing
              []

          ExSafe.try $
            AutoMigration.autoMigrateSchema
              AutoMigration.defaultOptions
              [AutoMigration.SchemaTable tableWithSystemAttributeNames]

    case result of
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "42701")
      Right () -> do
        HH.annotate "Expected migration to fail, but it did not"
        HH.failure

{- | Migration Guide: @SomeField@ has been removed. @foldMarshallerFields@ can be
  used to collect data from the fields in a @SqlMarshaller@ while converting
  the results to whatever type you desire.

@since 1.0.0.0
-}
data SomeField where
  SomeField :: Orville.FieldDefinition nullability a -> SomeField

describeField :: SomeField -> String
describeField (SomeField field) =
  B8.unpack (RawSql.toExampleBytes $ Orville.fieldColumnDefinition field)

prop_altersColumnDataType :: Orville.ConnectionPool -> HH.Property
prop_altersColumnDataType pool =
  HH.property $ do
    let
      baseFieldDefs =
        -- Serial columns are omitted from this list currently because
        -- the are pseudo-types in postgresql, rather than real column types.
        -- We don't handle migrating "away" from them to regular integer type.
        --
        -- time-like and boolean columns are omitted because postgresql raises
        -- an unable-to-cast error when attempting to migrate between them and
        -- numeric columns.
        [ SomeField $ Orville.unboundedTextField "column"
        , SomeField $ Orville.boundedTextField "column" 1
        , SomeField $ Orville.boundedTextField "column" 255
        , SomeField $ Orville.fixedTextField "column" 1
        , SomeField $ Orville.fixedTextField "column" 255
        , SomeField $ Orville.integerField "column"
        , SomeField $ Orville.smallIntegerField "column"
        , SomeField $ Orville.bigIntegerField "column"
        , SomeField $ Orville.doubleField "column"
        ]

      mkNullable (SomeField field) =
        case Orville.fieldNullability field of
          Orville.NullableField nullable ->
            SomeField $ nullable
          Orville.NotNullField notNull ->
            SomeField $ Orville.nullableField notNull

      generateFieldDefinition =
        Gen.element (baseFieldDefs ++ fmap mkNullable baseFieldDefs)

    SomeField originalField <- HH.forAllWith describeField generateFieldDefinition
    SomeField newField <- HH.forAllWith describeField generateFieldDefinition

    let
      originalTableDef =
        Orville.mkTableDefinitionWithoutKey
          "migration_test"
          (Orville.marshallField id originalField)

      newTableDef =
        Orville.mkTableDefinitionWithoutKey
          "migration_test"
          (Orville.marshallField id newField)

      newSqlType =
        Orville.fieldType newField

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    newTableDesc <- PgAssert.assertTableExists pool "migration_test"
    attr <- PgAssert.assertColumnExists newTableDesc "column"
    PgCatalog.pgAttributeTypeOid attr === Orville.sqlTypeOid newSqlType
    PgCatalog.pgAttributeMaxLength attr === Orville.sqlTypeMaximumLength newSqlType
    PgCatalog.pgAttributeIsNotNull attr === Orville.fieldIsNotNullable newField

genFieldWithMaybeDefault ::
  HH.Gen a ->
  (a -> Orville.DefaultValue a) ->
  Orville.FieldDefinition nullability a ->
  HH.Gen (Orville.FieldDefinition nullability a)
genFieldWithMaybeDefault defaultGen mkDefaultValue fieldDef = do
  maybeDefault <- Gen.maybe defaultGen
  pure $
    case maybeDefault of
      Nothing ->
        fieldDef
      Just def ->
        Orville.setDefaultValue (mkDefaultValue def) fieldDef

prop_altersColumnDefaultValue_TextNumeric :: Orville.ConnectionPool -> HH.Property
prop_altersColumnDefaultValue_TextNumeric pool =
  HH.property $ do
    let
      genDefaultText =
        PgGen.pgText (Range.linear 0 10)

      genDefaultIntegral :: (Integral n, Bounded n) => HH.Gen n
      genDefaultIntegral =
        Gen.integral Range.linearBounded

      genDefaultDouble =
        PgGen.pgDouble

    assertDefaultValuesMigrateProperly pool $
      Gen.choice
        [ SomeField <$> genFieldWithMaybeDefault genDefaultText Orville.textDefault (Orville.unboundedTextField "column")
        , SomeField <$> genFieldWithMaybeDefault genDefaultText Orville.textDefault (Orville.boundedTextField "column" 10)
        , SomeField <$> genFieldWithMaybeDefault genDefaultText Orville.textDefault (Orville.fixedTextField "column" 10)
        , SomeField <$> genFieldWithMaybeDefault genDefaultIntegral Orville.integerDefault (Orville.integerField "column")
        , SomeField <$> genFieldWithMaybeDefault genDefaultIntegral Orville.smallIntegerDefault (Orville.smallIntegerField "column")
        , SomeField <$> genFieldWithMaybeDefault genDefaultIntegral Orville.bigIntegerDefault (Orville.bigIntegerField "column")
        , SomeField <$> genFieldWithMaybeDefault genDefaultDouble Orville.doubleDefault (Orville.doubleField "column")
        ]

prop_altersColumnDefaultValue_IntegralBoundaries :: Orville.ConnectionPool -> HH.Property
prop_altersColumnDefaultValue_IntegralBoundaries pool =
  HH.property $ do
    let
      int32Min, int32Max :: Int64
      int32Min = fromIntegral (minBound :: Int32)
      int32Max = fromIntegral (maxBound :: Int32)

      genBoundary =
        Gen.element
          [ minBound
          , int32Min - 1
          , int32Min
          , -1
          , 0
          , 1
          , int32Max
          , int32Max + 1
          , maxBound
          ]

    assertDefaultValuesMigrateProperly pool $
      SomeField <$> genFieldWithMaybeDefault genBoundary Orville.bigIntegerDefault (Orville.bigIntegerField "column")

prop_altersColumnAddIdentity :: Orville.ConnectionPool -> HH.Property
prop_altersColumnAddIdentity pool =
  HH.property $ do
    colIdentity <- HH.forAll Gen.enumBounded

    let
      originalField = Orville.integerField "column"
      newField = Orville.markAsIdentity colIdentity originalField
      originalTableDef =
        Orville.mkTableDefinitionWithoutKey
          "migration_test"
          (Orville.marshallField id originalField)

      newTableDef =
        Orville.mkTableDefinitionWithoutKey
          "migration_test"
          (Orville.marshallField id newField)

    firstTimePlan <-
      HH.evalIO
        . Orville.runOrville pool
        $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    originalTableDesc <- PgAssert.assertTableExists pool "migration_test"

    secondTimePlan <-
      HH.evalIO
        . Orville.runOrville pool
        $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    PgAssert.assertFieldIdentityGenerationMatches originalTableDesc "column" Nothing
    newTableDesc <- PgAssert.assertTableExists pool "migration_test"
    PgAssert.assertFieldIdentityGenerationMatches newTableDesc "column" (Just colIdentity)
    migrationPlanStepStrings secondTimePlan === []

prop_altersColumnDefaultValue_Bool :: Orville.ConnectionPool -> HH.Property
prop_altersColumnDefaultValue_Bool pool =
  HH.property $ do
    assertDefaultValuesMigrateProperly pool $
      Gen.choice
        [ SomeField <$> genFieldWithMaybeDefault Gen.bool Orville.booleanDefault (Orville.booleanField "column")
        ]

prop_altersColumnDefaultValue_Timelike :: Orville.ConnectionPool -> HH.Property
prop_altersColumnDefaultValue_Timelike pool =
  HH.property $ do
    assertDefaultValuesMigrateProperly pool $
      Gen.choice
        [ -- Fields without default, or with specific times for the default
          SomeField <$> genFieldWithMaybeDefault PgGen.pgUTCTime Orville.utcTimestampDefault (Orville.utcTimestampField "column")
        , SomeField <$> genFieldWithMaybeDefault PgGen.pgLocalTime Orville.localTimestampDefault (Orville.localTimestampField "column")
        , SomeField <$> genFieldWithMaybeDefault PgGen.pgDay Orville.dateDefault (Orville.dateField "column")
        , -- Fields with "now" for the default
          pure . SomeField $ Orville.setDefaultValue Orville.currentUTCTimestampDefault (Orville.utcTimestampField "column")
        , pure . SomeField $ Orville.setDefaultValue Orville.currentLocalTimestampDefault (Orville.localTimestampField "column")
        , pure . SomeField $ Orville.setDefaultValue Orville.currentDateDefault (Orville.dateField "column")
        ]

prop_respectsImplicitDefaultOnSerialFields :: Orville.ConnectionPool -> HH.Property
prop_respectsImplicitDefaultOnSerialFields pool =
  HH.property $ do
    SomeField fieldDef <-
      HH.forAllWith describeField $
        Gen.element
          [ SomeField $ Orville.serialField "column"
          , SomeField $ Orville.bigSerialField "column"
          ]

    let
      tableDef =
        Orville.mkTableDefinitionWithoutKey
          "migration_test"
          (Orville.marshallField id fieldDef)

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql tableDef
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr tableDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable tableDef]

    originalTableDesc <- PgAssert.assertTableExists pool "migration_test"
    PgAssert.assertColumnDefaultExists originalTableDesc "column"

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable tableDef]

    newTableDesc <- PgAssert.assertTableExists pool "migration_test"
    PgAssert.assertColumnDefaultExists newTableDesc "column"
    migrationPlanStepStrings secondTimePlan === []

assertDefaultValuesMigrateProperly ::
  Orville.ConnectionPool ->
  HH.Gen SomeField ->
  HH.PropertyT IO ()
assertDefaultValuesMigrateProperly pool genSomeField = do
  SomeField originalField <- HH.forAllWith describeField genSomeField
  SomeField newField <- HH.forAllWith describeField genSomeField

  let
    originalTableDef =
      Orville.mkTableDefinitionWithoutKey
        "migration_test"
        (Orville.marshallField id originalField)

    newTableDef =
      Orville.mkTableDefinitionWithoutKey
        "migration_test"
        (Orville.marshallField id newField)

  firstTimePlan <-
    HH.evalIO $
      Orville.runOrville pool $ do
        Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
        Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr originalTableDef
        AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

  originalTableDesc <- PgAssert.assertTableExists pool "migration_test"
  PgAssert.assertColumnDefaultMatches originalTableDesc "column" (Orville.fieldDefaultValue originalField)

  secondTimePlan <-
    HH.evalIO $
      Orville.runOrville pool $ do
        AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
        AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

  newTableDesc <- PgAssert.assertTableExists pool "migration_test"
  PgAssert.assertColumnDefaultMatches newTableDesc "column" (Orville.fieldDefaultValue newField)
  migrationPlanStepStrings secondTimePlan === []

prop_addAndRemovesUniqueConstraints :: Orville.ConnectionPool -> HH.Property
prop_addAndRemovesUniqueConstraints pool =
  HH.property $ do
    let
      genColumnList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

      genConstraintColumns :: [String] -> HH.Gen [NEL.NonEmpty String]
      genConstraintColumns columns =
        fmap Maybe.catMaybes $
          Gen.list (Range.linear 0 10) $ do
            subcolumns <- Gen.subsequence columns
            NEL.nonEmpty <$> Gen.shuffle subcolumns

    originalColumns <- HH.forAll genColumnList
    originalConstraintColumns <- HH.forAll $ genConstraintColumns originalColumns
    newColumns <- HH.forAll genColumnList
    newConstraintColumns <- HH.forAll $ genConstraintColumns newColumns

    let
      columnsToDrop =
        originalColumns \\ newColumns

      originalConstraints =
        fmap mkUniqueConstraint originalConstraintColumns

      newConstraints =
        fmap mkUniqueConstraint newConstraintColumns

      originalTableDef =
        Orville.addTableConstraints originalConstraints $
          mkIntListTable "migration_test" originalColumns

      newTableDef =
        Orville.addTableConstraints newConstraints $
          Orville.dropColumns columnsToDrop $
            mkIntListTable "migration_test" newColumns

    HH.cover 5 (String.fromString "Adding Constraints") (not $ null (newConstraintColumns \\ originalConstraintColumns))
    HH.cover 5 (String.fromString "Dropping Constraints") (not $ null (originalConstraintColumns \\ newConstraintColumns))

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable originalTableDef]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"

    Fold.traverse_ (PgAssert.assertUniqueConstraintExists tableDesc) newConstraintColumns
    -- We ignore NotNullConstraint because it isn't supported on PostgreSQL versions < 18
    length
      ( filter
          (\a -> PgCatalog.pgConstraintType (PgCatalog.constraintRecord a) /= PgCatalog.NotNullConstraint)
          $ PgCatalog.relationConstraints tableDesc
      )
      === length (List.nub newConstraintColumns)

prop_addAndRemovesForeignKeyConstraints :: Orville.ConnectionPool -> HH.Property
prop_addAndRemovesForeignKeyConstraints pool =
  HH.property $ do
    let
      genColumnList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    localColumns <- HH.forAll genColumnList
    foreignColumns <- HH.forAll genColumnList

    let
      genForeignKeyInfos :: HH.Gen [PgAssert.ForeignKeyInfo]
      genForeignKeyInfos =
        fmap Maybe.catMaybes $
          Gen.list (Range.linear 0 10) $ do
            shuffledLocal <- Gen.shuffle localColumns
            shuffledForeign <- Gen.shuffle foreignColumns

            references <- Gen.subsequence (zip shuffledLocal shuffledForeign)
            onUpdateAction <- generateForeignKeyAction
            onDeleteAction <- generateForeignKeyAction

            pure $
              PgAssert.ForeignKeyInfo
                <$> NEL.nonEmpty references
                <*> Just onUpdateAction
                <*> Just onDeleteAction

    originalForeignKeyInfos <- HH.forAll genForeignKeyInfos
    newForeignKeyInfos <- HH.forAll genForeignKeyInfos

    -- We sort the columns in the unique constraints here to avoid edge cases
    -- with equivalent unique constraints in a different order. In these
    -- situations PostgreSQL can end up creating foreign keys that depending on
    -- indexes with a different ordering than the foreign key, which this test
    -- would then assume can be dropped even though it cannot. Standardizing
    -- the order of the columns in the unique constraints ensures this test
    -- will not try to drop a constraint that is used in both the original and
    -- new schemas while a foreign key is dropped and a new one added.
    let
      originalUniqueConstraints =
        fmap
          (mkUniqueConstraint . NEL.sort . fmap snd . PgAssert.foreignKeyInfoReferences)
          originalForeignKeyInfos

      newUniqueConstraints =
        fmap
          (mkUniqueConstraint . NEL.sort . fmap snd . PgAssert.foreignKeyInfoReferences)
          newForeignKeyInfos

      originalForeignKeyConstraints =
        fmap (mkForeignKeyConstraint "migration_test_foreign") originalForeignKeyInfos

      newForeignKeyConstraints =
        fmap (mkForeignKeyConstraint "migration_test_foreign") newForeignKeyInfos

      originalLocalTableDef =
        Orville.addTableConstraints originalForeignKeyConstraints $
          mkIntListTable "migration_test" localColumns

      newLocalTableDef =
        Orville.addTableConstraints newForeignKeyConstraints $
          mkIntListTable "migration_test" localColumns

      originalForeignTableDef =
        Orville.addTableConstraints originalUniqueConstraints $
          mkIntListTable "migration_test_foreign" foreignColumns

      newForeignTableDef =
        Orville.addTableConstraints newUniqueConstraints $
          mkIntListTable "migration_test_foreign" foreignColumns

    originalSchema <-
      HH.forAllWith (show . map AutoMigration.schemaItemSummary) $
        Gen.shuffle
          [ AutoMigration.SchemaTable originalForeignTableDef
          , AutoMigration.SchemaTable originalLocalTableDef
          ]

    newSchema <-
      HH.forAllWith (show . map AutoMigration.schemaItemSummary) $
        Gen.shuffle
          [ AutoMigration.SchemaTable newForeignTableDef
          , AutoMigration.SchemaTable newLocalTableDef
          ]

    HH.cover 5 (String.fromString "Adding Constraints") (not $ null (newForeignKeyInfos \\ originalForeignKeyInfos))
    HH.cover 5 (String.fromString "Dropping Constraints") (not $ null (originalForeignKeyInfos \\ newForeignKeyInfos))

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalLocalTableDef
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalForeignTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions originalSchema
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions newSchema

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions newSchema

    HH.annotate ("Second time migration steps: " <> show (migrationPlanStepStrings secondTimePlan))

    migrationPlanStepStrings secondTimePlan === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    Fold.traverse_ (PgAssert.assertForeignKeyConstraintExists tableDesc) newForeignKeyInfos
    -- We ignore NotNullConstraint because it isn't supported on PostgreSQL versions < 18
    length
      ( filter
          (\a -> PgCatalog.pgConstraintType (PgCatalog.constraintRecord a) /= PgCatalog.NotNullConstraint)
          $ PgCatalog.relationConstraints tableDesc
      )
      === length (List.nub newForeignKeyInfos)

prop_addAndRemovesCheckConstraints :: Orville.ConnectionPool -> HH.Property
prop_addAndRemovesCheckConstraints pool =
  HH.property $ do
    let
      genConstrList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    originalConstrs <- HH.forAll genConstrList
    newConstrs <- HH.forAll genConstrList

    let
      originalConstraints =
        fmap mkCheckConstraint originalConstrs

      newConstraints =
        fmap mkCheckConstraint newConstrs

      originalTableDef =
        Orville.addTableConstraints originalConstraints $
          mkIntListTable "migration_check_test" ["col1", "col2"]

      newTableDef =
        Orville.addTableConstraints newConstraints $
          mkIntListTable "migration_check_test" ["col1"]

    HH.cover 5 (String.fromString "Adding and Dropping Constraints") (not $ null (newConstrs \\ originalConstrs))

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable originalTableDef]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    tableDesc <- PgAssert.assertTableExists pool "migration_check_test"

    Fold.traverse_ (PgAssert.assertCheckConstraintExists tableDesc) newConstrs
    -- We ignore NotNullConstraint because it isn't supported on PostgreSQL versions < 18
    length
      ( filter
          (\a -> PgCatalog.pgConstraintType (PgCatalog.constraintRecord a) /= PgCatalog.NotNullConstraint)
          $ PgCatalog.relationConstraints tableDesc
      )
      === length newConstraints

prop_addNamedUniqueConstraint :: Orville.ConnectionPool -> HH.Property
prop_addNamedUniqueConstraint pool =
  HH.property $ do
    let
      genColumnList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

      genConstraintColumns :: HH.Gen ([String], [String], NEL.NonEmpty String, NEL.NonEmpty String)
      genConstraintColumns = do
        originalColumns <- Gen.filter (not . null) genColumnList
        common <- Gen.element originalColumns
        newColumns <- fmap (common :) genColumnList
        subcolumns <- Gen.subsequence $ List.intersect originalColumns newColumns
        originalConstraints <- Gen.just $ NEL.nonEmpty <$> Gen.shuffle subcolumns
        newConstraints <- Gen.just $ NEL.nonEmpty <$> Gen.shuffle subcolumns
        pure (originalColumns, newColumns, originalConstraints, newConstraints)

    (originalColumns, newColumns, originalConstraintColumns, newConstraintColumns) <- HH.forAll genConstraintColumns

    let
      constraintName = "constraint_name"

      columnsToDrop =
        originalColumns \\ newColumns

      originalConstraint =
        mkNamedUniqueConstraint constraintName originalConstraintColumns

      newConstraint =
        mkNamedUniqueConstraint constraintName newConstraintColumns

      originalTableDef =
        Orville.addTableConstraints [originalConstraint] $
          mkIntListTable "migration_test" originalColumns

      newTableDef =
        Orville.addTableConstraints [newConstraint] $
          Orville.dropColumns columnsToDrop $
            mkIntListTable "migration_test" newColumns

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable originalTableDef]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"

    -- We use originalConstraintColumns as we expect the constraint not to be migrated as the name is the same
    PgAssert.assertUniqueConstraintExists tableDesc originalConstraintColumns
    -- We ignore NotNullConstraint because it isn't supported on PostgreSQL versions < 18
    length
      ( filter
          (\a -> PgCatalog.pgConstraintType (PgCatalog.constraintRecord a) /= PgCatalog.NotNullConstraint)
          $ PgCatalog.relationConstraints tableDesc
      )
      === 1

prop_addsAndRemovesMixedIndexes :: Orville.ConnectionPool -> HH.Property
prop_addsAndRemovesMixedIndexes pool =
  HH.property $ do
    let
      genColumnList =
        Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    originalColumns <- HH.forAll genColumnList
    originalTestIndexes <- HH.forAll $ generateTestIndexes originalColumns "migration_test"
    newColumns <- HH.forAll genColumnList
    newTestIndexes <- HH.forAll $ generateTestIndexes newColumns "migration_test"

    let
      columnsToDrop =
        originalColumns \\ newColumns

      originalIndexes =
        fmap mkIndexDefinition originalTestIndexes

      newIndexes =
        fmap mkIndexDefinition newTestIndexes

      originalTableDef =
        Orville.addTableIndexes originalIndexes $
          mkIntListTable "migration_test" originalColumns

      originalTableDefWithSchema =
        TableDefinition.setTableSchema "orville_migration_schema" $
          Orville.addTableIndexes originalIndexes $
            mkIntListTable "migration_test" originalColumns

      newTableDef =
        Orville.addTableIndexes newIndexes $
          Orville.dropColumns columnsToDrop $
            mkIntListTable "migration_test" newColumns

      newTableDefWithSchema =
        TableDefinition.setTableSchema "orville_migration_schema" $
          Orville.addTableIndexes newIndexes $
            Orville.dropColumns columnsToDrop $
              mkIntListTable "migration_test" newColumns

    HH.cover 5 (String.fromString "Adding Indexes") (not $ null (newTestIndexes \\ originalTestIndexes))
    HH.cover 5 (String.fromString "Dropping Indexes") (not $ null (originalTestIndexes \\ newTestIndexes))
    HH.cover
      5
      (String.fromString "Concurrent Indexes")
      ( any
          (\i -> testIndexCreationStrategy i == Orville.Concurrent)
          (originalTestIndexes <> newTestIndexes)
      )

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP SCHEMA IF EXISTS orville_migration_schema CASCADE"
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE SCHEMA orville_migration_schema"
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable originalTableDef, AutoMigration.SchemaTable originalTableDefWithSchema]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef, AutoMigration.SchemaTable newTableDefWithSchema]

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    originalTableDesc <- PgAssert.assertTableExists pool "migration_test"
    Fold.traverse_
      (PgAssert.assertIndexExists originalTableDesc <$> testIndexUniqueness <*> testIndexColumns)
      originalTestIndexes

    originalTableWithSchemaDesc <- PgAssert.assertTableExistsInSchema pool "orville_migration_schema" "migration_test"
    Fold.traverse_
      (PgAssert.assertIndexExists originalTableWithSchemaDesc <$> testIndexUniqueness <*> testIndexColumns)
      originalTestIndexes

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan
            AutoMigration.defaultOptions
            [AutoMigration.SchemaTable newTableDef, AutoMigration.SchemaTable newTableDefWithSchema]

    migrationPlanStepStrings secondTimePlan === []

    newTableDesc <- PgAssert.assertTableExists pool "migration_test"
    newTableWithSchemaDesc <- PgAssert.assertTableExistsInSchema pool "orville_migration_schema" "migration_test"

    Fold.traverse_
      (PgAssert.assertIndexExists newTableDesc <$> testIndexUniqueness <*> testIndexColumns)
      newTestIndexes
    length (PgCatalog.relationIndexes newTableDesc) === length (List.nub newTestIndexes)

    Fold.traverse_
      (PgAssert.assertIndexExists newTableWithSchemaDesc <$> testIndexUniqueness <*> testIndexColumns)
      newTestIndexes
    length (PgCatalog.relationIndexes newTableWithSchemaDesc) === length (List.nub newTestIndexes)

prop_createsMissingSequences :: Orville.ConnectionPool -> HH.Property
prop_createsMissingSequences pool =
  Property.singletonProperty $ do
    let
      sequenceDef =
        Orville.mkSequenceDefinition "migration_test_sequence"
      sequenceId =
        Orville.sequenceIdentifier sequenceDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) (Orville.sequenceName sequenceDef)
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaSequence sequenceDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaSequence sequenceDef]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    _ <-
      PgAssert.assertSequenceExists
        pool
        (Orville.sequenceIdUnqualifiedNameString sequenceId)
    migrationPlanStepStrings secondTimePlan === []

prop_dropsRequestedSequences :: Orville.ConnectionPool -> HH.Property
prop_dropsRequestedSequences pool =
  Property.singletonProperty $ do
    let
      sequenceDef =
        Orville.mkSequenceDefinition "migration_test_sequence"
      sequenceId =
        Orville.sequenceIdentifier sequenceDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) (Orville.sequenceName sequenceDef)
          Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateSequenceExpr sequenceDef
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropSequence sequenceId]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropSequence sequenceId]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertSequenceDoesNotExist pool (Orville.sequenceIdUnqualifiedNameString sequenceId)
    migrationPlanStepStrings secondTimePlan === []

prop_altersModifiedSequences :: Orville.ConnectionPool -> HH.Property
prop_altersModifiedSequences pool =
  HH.property $ do
    let
      baseSequenceDef =
        Orville.mkSequenceDefinition "migration_test_sequence"
      generateIncrement =
        Gen.choice
          [ Gen.int64 (Range.linearFrom 1 1 maxBound)
          , Gen.int64 (Range.linearFrom (-1) minBound (-1))
          ]

    originalSequenceDef <- HH.forAll $ do
      increment <- generateIncrement
      minValue <- Gen.int64 (Range.linearFrom 0 minBound (maxBound - 1))
      maxValue <- Gen.int64 (Range.linear (minValue + 1) maxBound)
      start <- Gen.int64 (Range.linear minValue maxValue)
      cache <- Gen.int64 (Range.linear 1 maxBound)
      cycleFlag <- Gen.bool
      pure
        . Orville.setSequenceIncrement increment
        . Orville.setSequenceMinValue minValue
        . Orville.setSequenceMaxValue maxValue
        . Orville.setSequenceStart start
        . Orville.setSequenceCache cache
        . Orville.setSequenceCycle cycleFlag
        $ baseSequenceDef

    newSequenceDef <- HH.forAll $ do
      mbNewIncrement <- Gen.maybe generateIncrement
      -- The range between min and max values must contain the current next
      -- value of the sequence for PostgreSQL to not raise an error. Because
      -- no value has been fetched from our test sequence, that value is
      -- the start value from the original sequence definition
      mbNewMinValue <- Gen.maybe $ Gen.int64 (Range.linear minBound (Orville.sequenceStart originalSequenceDef))
      mbNewMaxValue <- Gen.maybe $ Gen.int64 (Range.linear (Orville.sequenceStart originalSequenceDef + 1) maxBound)

      -- The new start value must lie in the new range of the sequence. If no
      -- changes are being made to these values then the will remain the same as
      -- in the original sequence
      let
        newMinValue = Maybe.fromMaybe (Orville.sequenceMinValue originalSequenceDef) mbNewMinValue
        newMaxValue = Maybe.fromMaybe (Orville.sequenceMaxValue originalSequenceDef) mbNewMaxValue
      mbNewStart <- Gen.maybe $ Gen.int64 (Range.linear newMinValue newMaxValue)
      mbNewCache <- Gen.maybe $ Gen.int64 (Range.linear 1 maxBound)
      mbNewCycleFlag <- Gen.maybe Gen.bool
      pure
        . maybe id Orville.setSequenceIncrement mbNewIncrement
        . maybe id Orville.setSequenceMinValue mbNewMinValue
        . maybe id Orville.setSequenceMaxValue mbNewMaxValue
        . maybe id Orville.setSequenceStart mbNewStart
        . maybe id Orville.setSequenceCache mbNewCache
        . maybe id Orville.setSequenceCycle mbNewCycleFlag
        $ originalSequenceDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) (Orville.sequenceName originalSequenceDef)
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaSequence originalSequenceDef]

    HH.annotate ("First time steps: " <> show (migrationPlanStepStrings firstTimePlan))
    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaSequence newSequenceDef]

    HH.annotate ("Second time steps: " <> show (migrationPlanStepStrings secondTimePlan))
    assertSequenceExistsMatching pool originalSequenceDef

    thirdTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions secondTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaSequence newSequenceDef]

    assertSequenceExistsMatching pool newSequenceDef
    migrationPlanStepStrings thirdTimePlan === []

assertSequenceExistsMatching ::
  (HH.MonadTest m, MIO.MonadIO m) =>
  Orville.ConnectionPool ->
  Orville.SequenceDefinition ->
  m ()
assertSequenceExistsMatching pool sequenceDef = do
  sequenceRelation <-
    PgAssert.assertSequenceExists
      pool
      (Orville.sequenceIdUnqualifiedNameString . Orville.sequenceIdentifier $ sequenceDef)
  pgSequence <- PgAssert.assertRelationHasPgSequence sequenceRelation
  PgCatalog.pgSequenceIncrement pgSequence === Orville.sequenceIncrement sequenceDef
  PgCatalog.pgSequenceStart pgSequence === Orville.sequenceStart sequenceDef
  PgCatalog.pgSequenceMin pgSequence === Orville.sequenceMinValue sequenceDef
  PgCatalog.pgSequenceMax pgSequence === Orville.sequenceMaxValue sequenceDef
  PgCatalog.pgSequenceCache pgSequence === Orville.sequenceCache sequenceDef
  PgCatalog.pgSequenceCycle pgSequence === Orville.sequenceCycle sequenceDef

prop_arbitrarySchemaInitialMigration :: Orville.ConnectionPool -> HH.Property
prop_arbitrarySchemaInitialMigration pool =
  HH.property $ do
    testTables <- HH.forAll $ generateTestTables (Range.constant 0 10)

    HH.cover 75 (String.fromString "With Tables") (not . null $ testTables)
    HH.cover 75 (String.fromString "With Columns") (not . null $ concatMap testTableColumns testTables)
    HH.cover 50 (String.fromString "With Indexes") (not . null $ concatMap testTableIndexes testTables)
    HH.cover 50 (String.fromString "With Unique Constraints") (not . null $ concatMap testTableUniqueConstraints testTables)
    HH.cover 30 (String.fromString "With Foreign Keys") (not . null $ concatMap testTableForeignKeys testTables)

    let
      testSchema =
        map testTableSchemaItem testTables

    initialMigrationPlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP SCHEMA IF EXISTS orville_migration_test CASCADE"
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE SCHEMA orville_migration_test"
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions testSchema

    HH.annotate ("Initial migration steps: " <> show (migrationPlanStepStrings initialMigrationPlan))

    migrationPlanAfterMigration <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions initialMigrationPlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions testSchema

    migrationPlanStepStrings migrationPlanAfterMigration === []
    Fold.traverse_ (assertTableStructure pool) testTables

prop_createsMissingFunctions :: Orville.ConnectionPool -> HH.Property
prop_createsMissingFunctions pool =
  Property.singletonProperty $ do
    let
      functionDef =
        Orville.mkTriggerFunction
          "test_create_function"
          Orville.plpgsql
          "BEGIN return NEW; END"

      functionId =
        Orville.functionIdentifier functionDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $
            Expr.dropFunction
              (Just Expr.ifExists)
              (Orville.functionName functionDef)
              Nothing
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaFunction functionDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaFunction functionDef]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1

    proc <- PgAssert.assertFunctionExists pool (Orville.functionIdUnqualifiedNameString functionId)
    PgCatalog.pgProcSource proc === T.pack "BEGIN return NEW; END"

    migrationPlanStepStrings secondTimePlan === []

prop_recreatesAlteredFunctions :: Orville.ConnectionPool -> HH.Property
prop_recreatesAlteredFunctions pool =
  Property.singletonProperty $ do
    let
      oldFunctionDef =
        Orville.mkTriggerFunction
          "test_recreate_function"
          Orville.plpgsql
          "BEGIN return OLD; END"

      newFunctionDef =
        Orville.mkTriggerFunction
          "test_recreate_function"
          Orville.plpgsql
          "BEGIN return NEW; END"

      functionId =
        Orville.functionIdentifier oldFunctionDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $
            Expr.dropFunction (Just Expr.ifExists) (Orville.functionName oldFunctionDef) Nothing
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaFunction oldFunctionDef]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaFunction newFunctionDef]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaFunction newFunctionDef]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1

    proc <- PgAssert.assertFunctionExists pool (Orville.functionIdUnqualifiedNameString functionId)
    PgCatalog.pgProcSource proc === T.pack "BEGIN return NEW; END"

    migrationPlanStepStrings secondTimePlan === []

prop_dropsRequestedFunctions :: Orville.ConnectionPool -> HH.Property
prop_dropsRequestedFunctions pool =
  Property.singletonProperty $ do
    let
      functionDef =
        Orville.mkTriggerFunction
          "test_drop_function"
          Orville.plpgsql
          "BEGIN return NEW; END"

      functionId =
        Orville.functionIdentifier functionDef

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateFunctionExpr functionDef (Just Expr.orReplace)
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropFunction functionId]

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaDropFunction functionId]

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertFunctionDoesNotExist pool (Orville.functionIdUnqualifiedNameString functionId)
    migrationPlanStepStrings secondTimePlan === []

prop_createsMissingTriggers :: Orville.ConnectionPool -> HH.Property
prop_createsMissingTriggers pool =
  Property.singletonProperty $ do
    let
      functionDef =
        Orville.mkTriggerFunction
          "test_create_trigger_function"
          Orville.plpgsql
          "BEGIN return NEW; END"

      testTrigger =
        Orville.beforeInsert
          "before_insert_trigger"
          (Orville.functionName functionDef)

      tableWithTrigger =
        Orville.addTableTriggers [testTrigger] Foo.table

      schemaItems =
        [ AutoMigration.SchemaFunction functionDef
        , AutoMigration.SchemaTable tableWithTrigger
        ]

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql tableWithTrigger
          Orville.executeVoid Orville.DDLQuery $ Expr.dropFunction (Just Expr.ifExists) (Orville.functionName functionDef) Nothing
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItems

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItems

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 3

    fooRelation <- PgAssert.assertTableExists pool "foo"
    _ <- PgAssert.assertTriggerExists fooRelation "before_insert_trigger"

    migrationPlanStepStrings secondTimePlan === []

prop_dropsUnrequestedTriggers :: Orville.ConnectionPool -> HH.Property
prop_dropsUnrequestedTriggers pool =
  Property.singletonProperty $ do
    let
      functionDef =
        Orville.mkTriggerFunction
          "test_drop_trigger_function"
          Orville.plpgsql
          "BEGIN return NEW; END"

      testTrigger =
        Orville.beforeInsert
          "before_insert_trigger"
          (Orville.functionName functionDef)

      tableWithoutTrigger =
        Foo.table

      tableWithTrigger =
        Orville.addTableTriggers [testTrigger] tableWithoutTrigger

      schemaItemsWithTrigger =
        [ AutoMigration.SchemaFunction functionDef
        , AutoMigration.SchemaTable tableWithTrigger
        ]

      schemaItemsWithoutTrigger =
        [ AutoMigration.SchemaFunction functionDef
        , AutoMigration.SchemaTable tableWithoutTrigger
        ]

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql tableWithoutTrigger
          Orville.executeVoid Orville.DDLQuery $ Expr.dropFunction (Just Expr.ifExists) (Orville.functionName functionDef) Nothing
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions schemaItemsWithTrigger
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItemsWithoutTrigger

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItemsWithoutTrigger

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    fooRelation <- PgAssert.assertTableExists pool "foo"
    _ <- PgAssert.assertTriggerDoesNotExist fooRelation "before_insert_trigger"
    migrationPlanStepStrings secondTimePlan === []

-- Policy migration tests. These share a common shape: set up a starting
-- state, generate a plan for a target schema, assert the step count, execute
-- the plan and assert that a second plan for the same schema is empty. That
-- shape lives in 'assertPolicyMigrationConverges'.

policyTestTable :: Orville.TableDefinition Orville.NoKey Int32 Int32
policyTestTable =
  Orville.mkTableDefinitionWithoutKey
    "policy_migration_test"
    (Orville.marshallField id (Orville.integerField "column"))

policyTestTableWithName :: Orville.TableDefinition Orville.NoKey T.Text T.Text
policyTestTableWithName =
  Orville.mkTableDefinitionWithoutKey
    "policy_migration_test"
    (Orville.marshallField id (Orville.unboundedTextField "name"))

dropPolicyTestTable :: Orville.Orville ()
dropPolicyTestTable =
  Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql policyTestTable

autoMigrateTestSchema :: [AutoMigration.SchemaItem] -> Orville.Orville ()
autoMigrateTestSchema =
  AutoMigration.autoMigrateSchema AutoMigration.defaultOptions

mkTestPolicy :: String -> Orville.PolicyDefinition
mkTestPolicy name =
  Orville.mkPolicyDefinition name Nothing Nothing Nothing Nothing Nothing

{- | A test policy with a USING expression given as raw SQL. The SQL should be
  written to match the deparsed form PostgreSQL returns in pg_policies
  (including the parentheses PostgreSQL puts around operator expressions) so
  that matching policies produce empty plans.
-}
mkRawUsingPolicy :: String -> Orville.PolicyDefinition
mkRawUsingPolicy usingSql =
  Orville.mkPolicyDefinition
    "migration_test_policy"
    Nothing
    Nothing
    Nothing
    (Just . Expr.policyUsingExpr $ RawSql.unsafeSqlExpression usingSql)
    Nothing

{- | Runs the given setup actions, generates a migration plan for the target
  schema, asserts it has the expected number of steps, executes it, and
  asserts that a second plan for the same schema is empty.
-}
assertPolicyMigrationConverges ::
  Orville.ConnectionPool ->
  Orville.Orville () ->
  [AutoMigration.SchemaItem] ->
  Int ->
  HH.PropertyT IO ()
assertPolicyMigrationConverges pool setup targetSchema expectedStepCount = do
  plan <-
    HH.evalIO $
      Orville.runOrville pool $ do
        setup
        AutoMigration.generateMigrationPlan AutoMigration.defaultOptions targetSchema

  HH.annotate ("Migration steps: " <> show (migrationPlanStepStrings plan))
  length (AutoMigration.migrationPlanSteps plan) === expectedStepCount

  secondTimePlan <-
    HH.evalIO $
      Orville.runOrville pool $ do
        AutoMigration.executeMigrationPlan AutoMigration.defaultOptions plan
        AutoMigration.generateMigrationPlan AutoMigration.defaultOptions targetSchema

  HH.annotate ("Second time steps: " <> show (migrationPlanStepStrings secondTimePlan))
  migrationPlanStepStrings secondTimePlan === []

prop_createsMissingPolicies :: Orville.ConnectionPool -> HH.Property
prop_createsMissingPolicies pool =
  Property.singletonProperty $
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr policyTestTable
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      1

prop_dropsRequestedPolicies :: Orville.ConnectionPool -> HH.Property
prop_dropsRequestedPolicies pool =
  Property.singletonProperty $
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      )
      [AutoMigration.SchemaTable (Orville.dropPolicies ["migration_test_policy"] policyTestTable)]
      1

prop_recreatesModifiedPolicies :: Orville.ConnectionPool -> HH.Property
prop_recreatesModifiedPolicies pool =
  Property.singletonProperty $ do
    let
      modifiedPolicy =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          Nothing
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True)
          Nothing

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [modifiedPolicy] policyTestTable)]
      2

prop_createsRestrictivePolicies :: Orville.ConnectionPool -> HH.Property
prop_createsRestrictivePolicies pool =
  Property.singletonProperty $ do
    let
      restrictivePolicy =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          (Just Orville.PolicyRestrictive)
          Nothing
          Nothing
          Nothing
          Nothing

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr policyTestTable
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [restrictivePolicy] policyTestTable)]
      1

prop_recreatesPoliciesWithChangedPermission :: Orville.ConnectionPool -> HH.Property
prop_recreatesPoliciesWithChangedPermission pool =
  Property.singletonProperty $ do
    let
      restrictivePolicy =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          (Just Orville.PolicyRestrictive)
          Nothing
          Nothing
          Nothing
          Nothing

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [restrictivePolicy] policyTestTable)]
      2

prop_recreatesPoliciesWithChangedCommand :: Orville.ConnectionPool -> HH.Property
prop_recreatesPoliciesWithChangedCommand pool =
  Property.singletonProperty $ do
    let
      mkCommandPolicy command =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          (Just command)
          Nothing
          Nothing
          Nothing

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkCommandPolicy Orville.PolicyCommandSelect] policyTestTable)]
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [mkCommandPolicy Orville.PolicyCommandUpdate] policyTestTable)]
      2

prop_recreatesPoliciesWithRemovedExpressions :: Orville.ConnectionPool -> HH.Property
prop_recreatesPoliciesWithRemovedExpressions pool =
  Property.singletonProperty $ do
    let
      policyWithUsing =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          Nothing
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True)
          Nothing

    -- ALTER POLICY cannot remove a USING clause, so this change requires the
    -- policy to be dropped and recreated
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [policyWithUsing] policyTestTable)]
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      2

prop_recreatesPoliciesAcrossColumnChanges :: Orville.ConnectionPool -> HH.Property
prop_recreatesPoliciesAcrossColumnChanges pool =
  Property.singletonProperty $ do
    let
      twoColumnTable =
        Orville.mkTableDefinitionWithoutKey
          "policy_migration_test"
          ( (,)
              <$> Orville.marshallField fst (Orville.integerField "column")
              <*> Orville.marshallField snd (Orville.unboundedTextField "name")
          )
      tableWithNamePolicy =
        Orville.addTablePolicies [mkRawUsingPolicy "(name = 'abc'::text)"] twoColumnTable
      tableWithColumnPolicy =
        Orville.addTablePolicies
          [mkRawUsingPolicy "(\"column\" > 0)"]
          (Orville.dropColumns ["name"] policyTestTable)

    -- The policy drop must run before the column drop, and the recreate
    -- after it, or PostgreSQL rejects dropping a column the policy
    -- references. Expected steps: drop policy, drop column, create policy.
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable tableWithNamePolicy]
      )
      [AutoMigration.SchemaTable tableWithColumnPolicy]
      3

prop_invalidPolicyDefinitionsRaiseError :: Orville.ConnectionPool -> HH.Property
prop_invalidPolicyDefinitionsRaiseError pool =
  Property.singletonProperty $ do
    let
      usingTrue = Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True
      checkTrue = Just . Expr.policyCheckExpr $ Expr.literalBooleanExpr True

      invalidPolicies =
        [ -- USING is not allowed on INSERT policies
          Orville.mkPolicyDefinition "migration_test_policy" Nothing (Just Orville.PolicyCommandInsert) Nothing usingTrue Nothing
        , -- WITH CHECK is not allowed on SELECT policies
          Orville.mkPolicyDefinition "migration_test_policy" Nothing (Just Orville.PolicyCommandSelect) Nothing Nothing checkTrue
        , -- Names longer than PostgreSQL's 63-byte identifier limit would be truncated
          Orville.mkPolicyDefinition (List.replicate 70 'x') Nothing Nothing Nothing Nothing Nothing
        , -- Bind parameters cannot be used in DDL
          Orville.mkPolicyDefinition "migration_test_policy" Nothing Nothing Nothing (Just . Expr.policyUsingExpr $ Orville.fieldEquals (Orville.integerField "column") 1) Nothing
        ]

      assertRaisesInvalidPolicyError policyDef = do
        result <-
          HH.evalIO $
            Orville.runOrville pool $
              ExSafe.try $
                AutoMigration.generateMigrationPlan
                  AutoMigration.defaultOptions
                  [AutoMigration.SchemaTable (Orville.addTablePolicies [policyDef] policyTestTable)]

        case result of
          Left err -> do
            HH.annotate (show (err :: AutoMigration.MigrationDataError))
            HH.assert $ List.isInfixOf "InvalidPolicyDefinition" (show err)
          Right plan -> do
            HH.annotate ("Expected plan generation to fail, but got steps: " <> show (migrationPlanStepStrings plan))
            HH.failure

    HH.evalIO $ Orville.runOrville pool dropPolicyTestTable
    Fold.traverse_ assertRaisesInvalidPolicyError invalidPolicies

prop_normalizesPublicRoleTargets :: Orville.ConnectionPool -> HH.Property
prop_normalizesPublicRoleTargets pool =
  Property.singletonProperty $ do
    let
      mkRolesPolicy roles =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          (Just $ Set.fromList roles)
          Nothing
          Nothing

    -- PostgreSQL treats a quoted "public" role target as the PUBLIC
    -- pseudo-role, and ignores other targets given alongside PUBLIC
    Orville.policyDefinitionPolicyRoles (mkRolesPolicy [Orville.PolicyRoleNamed "public"])
      === Set.singleton Orville.PolicyRolePublic
    Orville.policyDefinitionPolicyRoles (mkRolesPolicy [Orville.PolicyRolePublic, Orville.PolicyRoleNamed "orville_test"])
      === Set.singleton Orville.PolicyRolePublic

    -- A definition mixing PUBLIC with named roles must converge rather than
    -- fighting PostgreSQL's collapse of the role list forever
    let
      tableWithMixedRoles =
        Orville.addTablePolicies
          [mkRolesPolicy [Orville.PolicyRolePublic, Orville.PolicyRoleNamed "orville_test"]]
          policyTestTable

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable tableWithMixedRoles]
      )
      [AutoMigration.SchemaTable tableWithMixedRoles]
      0

prop_managesPoliciesOnSchemaQualifiedTables :: Orville.ConnectionPool -> HH.Property
prop_managesPoliciesOnSchemaQualifiedTables pool =
  Property.singletonProperty $ do
    let
      qualifiedTable =
        Orville.setTableSchema "orville_migration_schema" policyTestTable

    assertPolicyMigrationConverges
      pool
      ( do
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP SCHEMA IF EXISTS orville_migration_schema CASCADE"
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE SCHEMA orville_migration_schema"
          Orville.executeVoid Orville.DDLQuery $ Schema.mkCreateTableExpr qualifiedTable
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] qualifiedTable)]
      1

prop_enablesRowLevelSecurityWithoutPolicyCreation :: Orville.ConnectionPool -> HH.Property
prop_enablesRowLevelSecurityWithoutPolicyCreation pool =
  Property.singletonProperty $
    -- The expected steps are the policy drop plus the RLS enable
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable)]
      )
      [ AutoMigration.SchemaTable
          ( Orville.setRowLevelSecurityEnabled $
              Orville.dropPolicies ["migration_test_policy"] policyTestTable
          )
      ]
      2

prop_disablesRowLevelSecurityWhenNotRequested :: Orville.ConnectionPool -> HH.Property
prop_disablesRowLevelSecurityWhenNotRequested pool =
  Property.singletonProperty $
    -- Once RLS is disabled the second plan must be empty: no DISABLE
    -- statement is emitted for a table whose relrowsecurity is already false
    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.setRowLevelSecurityEnabled policyTestTable)]
      )
      [AutoMigration.SchemaTable policyTestTable]
      1

prop_altersPoliciesWhoseLiteralsDifferOnlyByCase :: Orville.ConnectionPool -> HH.Property
prop_altersPoliciesWhoseLiteralsDifferOnlyByCase pool =
  Property.singletonProperty $ do
    let
      mkNamePolicy literal =
        mkRawUsingPolicy ("(name = '" <> literal <> "'::text)")

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema [AutoMigration.SchemaTable (Orville.addTablePolicies [mkNamePolicy "abc"] policyTestTableWithName)]
      )
      [AutoMigration.SchemaTable (Orville.addTablePolicies [mkNamePolicy "ABC"] policyTestTableWithName)]
      2

prop_managesPolicyRoleTargets :: Orville.ConnectionPool -> HH.Property
prop_managesPolicyRoleTargets pool =
  Property.singletonProperty $ do
    let
      mkRolePolicy role =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          (Just $ Set.singleton role)
          Nothing
          Nothing
      -- "orville_test" is the role the test suite connects as, so it is
      -- guaranteed to exist
      namedRoleSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkRolePolicy (Orville.PolicyRoleNamed "orville_test")] policyTestTable)]
      publicSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkRolePolicy Orville.PolicyRolePublic] policyTestTable)]

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema namedRoleSchema
      )
      namedRoleSchema
      0

    assertPolicyMigrationConverges pool (pure ()) publicSchema 2

prop_managesPoliciesWithCheckExprs :: Orville.ConnectionPool -> HH.Property
prop_managesPoliciesWithCheckExprs pool =
  Property.singletonProperty $ do
    let
      -- Written to match the deparsed form PostgreSQL returns in pg_policies
      mkCheckPolicy literal =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          Nothing
          Nothing
          (Just . Expr.policyCheckExpr . RawSql.unsafeSqlExpression $ "(name = '" <> literal <> "'::text)")
      abcSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkCheckPolicy "abc"] policyTestTableWithName)]
      xyzSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkCheckPolicy "xyz"] policyTestTableWithName)]

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema abcSchema
      )
      abcSchema
      0

    assertPolicyMigrationConverges pool (pure ()) xyzSchema 2

prop_managesPoliciesWithMultipleRoles :: Orville.ConnectionPool -> HH.Property
prop_managesPoliciesWithMultipleRoles pool =
  Property.singletonProperty $ do
    let
      multiRolePolicy =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          ( Just $
              Set.fromList
                [ Orville.PolicyRoleNamed "orville_test"
                , Orville.PolicyRoleNamed "orville_migration_role"
                ]
          )
          Nothing
          Nothing
      tableWithPolicy =
        Orville.addTablePolicies [multiRolePolicy] policyTestTable

    assertPolicyMigrationConverges
      pool
      ( do
          -- The table (and with it any policy referencing the role) must be
          -- dropped before the role so the role drop doesn't fail on a
          -- dependency from a previous test run
          dropPolicyTestTable
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP ROLE IF EXISTS orville_migration_role"
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE ROLE orville_migration_role"
          autoMigrateTestSchema [AutoMigration.SchemaTable tableWithPolicy]
      )
      [AutoMigration.SchemaTable tableWithPolicy]
      0

prop_managesPoliciesWithQuotedRoleNames :: Orville.ConnectionPool -> HH.Property
prop_managesPoliciesWithQuotedRoleNames pool =
  Property.singletonProperty $ do
    let
      -- The double quote, comma and spaces force PostgreSQL to render this
      -- role quoted and escaped in the pg_policies roles array, alongside the
      -- unquoted orville_test element
      weirdRoleName = "orville \"quoted\", role"
      quotedRolePolicy =
        Orville.mkPolicyDefinition
          "migration_test_policy"
          Nothing
          Nothing
          ( Just $
              Set.fromList
                [ Orville.PolicyRoleNamed weirdRoleName
                , Orville.PolicyRoleNamed "orville_test"
                ]
          )
          Nothing
          Nothing
      tableWithPolicy =
        Orville.addTablePolicies [quotedRolePolicy] policyTestTable

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "DROP ROLE IF EXISTS \"orville \"\"quoted\"\", role\""
          Orville.executeVoid Orville.DDLQuery $ RawSql.fromString "CREATE ROLE \"orville \"\"quoted\"\", role\""
          autoMigrateTestSchema [AutoMigration.SchemaTable tableWithPolicy]
      )
      [AutoMigration.SchemaTable tableWithPolicy]
      0

prop_conflictingPolicyDefinitionsRaiseError :: Orville.ConnectionPool -> HH.Property
prop_conflictingPolicyDefinitionsRaiseError pool =
  Property.singletonProperty $ do
    let
      conflictedTableDef =
        Orville.dropPolicies ["migration_test_policy"] $
          Orville.addTablePolicies [mkTestPolicy "migration_test_policy"] policyTestTable

    result <-
      HH.evalIO $
        Orville.runOrville pool $ do
          dropPolicyTestTable
          ExSafe.try $
            AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable conflictedTableDef]

    case result of
      Left err -> do
        HH.annotate (show (err :: AutoMigration.MigrationDataError))
        HH.assert $ List.isInfixOf "migration_test_policy" (show err)
      Right plan -> do
        HH.annotate ("Expected plan generation to fail, but got steps: " <> show (migrationPlanStepStrings plan))
        HH.failure

prop_managesMultiplePoliciesOnOneTable :: Orville.ConnectionPool -> HH.Property
prop_managesMultiplePoliciesOnOneTable pool =
  Property.singletonProperty $ do
    let
      recreatedPolicy =
        Orville.mkPolicyDefinition
          "migration_policy_recreate"
          Nothing
          Nothing
          Nothing
          (Just . Expr.policyUsingExpr $ Expr.literalBooleanExpr True)
          Nothing

      originalSchema =
        [ AutoMigration.SchemaTable
            (Orville.addTablePolicies [mkTestPolicy "migration_policy_recreate", mkTestPolicy "migration_policy_drop"] policyTestTable)
        ]
      -- One plan must recreate migration_policy_recreate (two steps), drop
      -- migration_policy_drop and create migration_policy_create
      newSchema =
        [ AutoMigration.SchemaTable
            ( Orville.dropPolicies ["migration_policy_drop"] $
                Orville.addTablePolicies [recreatedPolicy, mkTestPolicy "migration_policy_create"] policyTestTable
            )
        ]

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema originalSchema
      )
      newSchema
      4

prop_altersPoliciesWithQuotedIdentifiers :: Orville.ConnectionPool -> HH.Property
prop_altersPoliciesWithQuotedIdentifiers pool =
  Property.singletonProperty $ do
    let
      -- Two columns whose names differ only by case, both requiring quoting
      -- in the deparsed policy expressions
      quotedColumnsTable =
        Orville.mkTableDefinitionWithoutKey
          "policy_migration_test"
          ( (,)
              <$> Orville.marshallField fst (Orville.unboundedTextField "UserId")
              <*> Orville.marshallField snd (Orville.unboundedTextField "USERID")
          )
      mkIdentPolicy columnName =
        mkRawUsingPolicy ("(\"" <> columnName <> "\" = 'x'::text)")
      mixedCaseSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkIdentPolicy "UserId"] quotedColumnsTable)]
      upperCaseSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkIdentPolicy "USERID"] quotedColumnsTable)]

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema mixedCaseSchema
      )
      mixedCaseSchema
      0

    assertPolicyMigrationConverges pool (pure ()) upperCaseSchema 2

prop_altersPoliciesWithEscapedQuoteLiterals :: Orville.ConnectionPool -> HH.Property
prop_altersPoliciesWithEscapedQuoteLiterals pool =
  Property.singletonProperty $ do
    let
      -- The literals contain an escaped quote ('') followed by a letter that
      -- differs only by case, so a comparison that mishandled the escaped
      -- quote would treat the letter as being outside the literal and fold it
      mkQuotePolicy literal =
        mkRawUsingPolicy ("(name = '" <> literal <> "'::text)")
      upperSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkQuotePolicy "it''S"] policyTestTableWithName)]
      lowerSchema =
        [AutoMigration.SchemaTable (Orville.addTablePolicies [mkQuotePolicy "it''s"] policyTestTableWithName)]

    assertPolicyMigrationConverges
      pool
      ( do
          dropPolicyTestTable
          autoMigrateTestSchema upperSchema
      )
      upperSchema
      0

    assertPolicyMigrationConverges pool (pure ()) lowerSchema 2

prop_addTablePoliciesAccumulates :: HH.Property
prop_addTablePoliciesAccumulates =
  Property.singletonProperty $ do
    let
      mkNamedPolicy name mbPermission =
        Orville.mkPolicyDefinition name mbPermission Nothing Nothing Nothing Nothing
      tableDef =
        Orville.addTablePolicies [mkNamedPolicy "policy_one" (Just Orville.PolicyRestrictive)] $
          Orville.addTablePolicies
            [ mkNamedPolicy "policy_one" Nothing
            , -- Duplicate names within a single call: the later list element wins
              mkNamedPolicy "policy_two" (Just Orville.PolicyRestrictive)
            , mkNamedPolicy "policy_two" Nothing
            ]
            policyTestTable
      policies = Orville.tablePolicies tableDef

    Map.keys policies === ["policy_one", "policy_two"]
    fmap Orville.policyDefinitionPermission (Map.lookup "policy_one" policies) === Just Orville.PolicyRestrictive
    fmap Orville.policyDefinitionPermission (Map.lookup "policy_two" policies) === Just Orville.PolicyPermissive

prop_loadsMissingExtensions :: Orville.ConnectionPool -> HH.Property
prop_loadsMissingExtensions pool =
  Property.singletonProperty $ do
    let
      schemaItems =
        [ AutoMigration.SchemaExtension $ Orville.nameToExtensionId "pg_trgm"
        ]

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid Orville.DDLQuery $ Expr.dropExtensionExpr (Expr.extensionName "pg_trgm") (Just Expr.ifExists) Nothing
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItems

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItems

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    _ <- PgAssert.assertExtensionLoaded pool "pg_trgm"
    migrationPlanStepStrings secondTimePlan === []

prop_unloadsPresentExtensions :: Orville.ConnectionPool -> HH.Property
prop_unloadsPresentExtensions pool =
  Property.singletonProperty $ do
    let
      pgtrgmExtension = Orville.nameToExtensionId "pg_trgm"

      schemaItemsLoadExtension =
        [ AutoMigration.SchemaExtension pgtrgmExtension
        ]

      schemaItemsUnloadExtension =
        [ AutoMigration.SchemaDropExtension pgtrgmExtension
        ]

    firstTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions schemaItemsLoadExtension
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItemsUnloadExtension

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions schemaItemsUnloadExtension

    length (AutoMigration.migrationPlanSteps firstTimePlan) === 1
    PgAssert.assertExtensionNotLoaded pool "pg_trgm"

    migrationPlanStepStrings secondTimePlan === []

assertTableStructure ::
  (HH.MonadTest m, MIO.MonadIO m) =>
  Orville.ConnectionPool ->
  TestTable ->
  m ()
assertTableStructure pool testTable = do
  tableDesc <- PgAssert.assertTableExistsInSchema pool "orville_migration_test" (testTableName testTable)

  PgAssert.assertColumnNamesEqual
    tableDesc
    (testTableColumns testTable)

  Fold.traverse_
    (PgAssert.assertIndexExists tableDesc <$> testIndexUniqueness <*> testIndexColumns)
    (testTableIndexes testTable)

  Fold.traverse_
    (PgAssert.assertUniqueConstraintExists tableDesc)
    (testTableUniqueConstraints testTable)

  Fold.traverse_
    (PgAssert.assertForeignKeyConstraintExists tableDesc . mkForeignKeyInfo)
    (testTableForeignKeys testTable)

mkForeignKeyInfo :: TestForeignKey -> PgAssert.ForeignKeyInfo
mkForeignKeyInfo testForeignKey =
  PgAssert.ForeignKeyInfo
    { PgAssert.foreignKeyInfoReferences = testForeignKeyReferences testForeignKey
    , PgAssert.foreignKeyInfoOnUpdate = testForeignKeyOnUpdate testForeignKey
    , PgAssert.foreignKeyInfoOnDelete = testForeignKeyOnDelete testForeignKey
    }

data TestTable = TestTable
  { testTableName :: String
  , testTableColumns :: [String]
  , testTablePrimaryKey :: [String]
  , testTableIndexes :: [TestIndex]
  , testTableUniqueConstraints :: [NEL.NonEmpty String]
  , testTableForeignKeys :: [TestForeignKey]
  }
  deriving (Show)

data TestIndex = TestIndex
  { testIndexName :: Maybe String
  , testIndexUniqueness :: Orville.IndexUniqueness
  , testIndexCreationStrategy :: Orville.IndexCreationStrategy
  , testIndexColumns :: NEL.NonEmpty String
  }
  deriving (Eq, Show)

data TestForeignKey = TestForeignKey
  { testForeignKeyReferences :: NEL.NonEmpty (String, String)
  , testForeignKeyTableName :: String
  , testForeignKeyOnUpdate :: Orville.ForeignKeyAction
  , testForeignKeyOnDelete :: Orville.ForeignKeyAction
  }
  deriving (Show)

data TestForeignKeyTarget = TestForeignKeyTarget
  { testForeignKeyTargetTableName :: String
  , testForeignKeyTargetColumns :: NEL.NonEmpty String
  }
  deriving (Show)

testTableForeignKeyTargets :: TestTable -> [TestForeignKeyTarget]
testTableForeignKeyTargets testTable =
  map
    (TestForeignKeyTarget $ testTableName testTable)
    (testTableUniqueConstraints testTable)

mkIndexDefinition :: TestIndex -> Orville.IndexDefinition
mkIndexDefinition testIndex =
  let
    strategy =
      testIndexCreationStrategy testIndex

    baseIndex =
      case testIndexName testIndex of
        Just name ->
          -- If the test index has a name, use it to test Orville's support for
          -- custome named indexes
          let
            indexBody =
              Expr.indexBodyColumns
                . fmap Expr.columnName
                . testIndexColumns
                $ testIndex
          in
            Orville.mkNamedIndexDefinition
              (testIndexUniqueness testIndex)
              name
              indexBody
        Nothing ->
          -- If the test index has no name, use it to test Orville's support for
          -- unnamed indexes
          Orville.mkIndexDefinition
            (testIndexUniqueness testIndex)
            (Orville.stringToFieldName <$> testIndexColumns testIndex)
  in
    Orville.setIndexCreationStrategy strategy baseIndex

generateTestTable :: HH.Gen TestTable
generateTestTable = do
  columns <- generateTestTableColumns

  tableName <- PgGen.pgIdentifierWithPrefix "t_" 1

  TestTable tableName columns
    <$> Gen.subsequence columns
    <*> generateTestIndexes columns tableName
    <*> generateTestUniqueConstraints columns
    <*> pure [] -- No foreign keys can be generated until we've generate test tables

generateTestTables :: HH.Range Int -> HH.Gen [TestTable]
generateTestTables tableCountRange = do
  tablesWithoutForeignKeys <-
    fmap
      (List.nubBy (Function.on (==) testTableName))
      (Gen.list tableCountRange generateTestTable)

  let
    foreignKeyTargets =
      concatMap testTableForeignKeyTargets tablesWithoutForeignKeys

  traverse
    (addTestTableForeignKeys foreignKeyTargets)
    tablesWithoutForeignKeys

addTestTableForeignKeys ::
  [TestForeignKeyTarget] ->
  TestTable ->
  HH.Gen TestTable
addTestTableForeignKeys targets table = do
  let
    targetColumnCount =
      length . testForeignKeyTargetColumns

    possibleTargets =
      filter
        (\t -> targetColumnCount t <= length (testTableColumns table))
        targets

    genForeignKey target = do
      let
        targetColumns = testForeignKeyTargetColumns target

      sourceColumns <-
        -- nonEmpty can never produce a 'Nothing' here because the target's
        -- columns are a non-empty list.
        Gen.mapMaybe
          NEL.nonEmpty
          (take (targetColumnCount target) <$> Gen.shuffle (testTableColumns table))

      onUpdateAction <- generateForeignKeyAction
      onDeleteAction <- generateForeignKeyAction

      pure $
        TestForeignKey
          { testForeignKeyReferences = NEL.zip sourceColumns targetColumns
          , testForeignKeyTableName = testForeignKeyTargetTableName target
          , testForeignKeyOnUpdate = onUpdateAction
          , testForeignKeyOnDelete = onDeleteAction
          }

  chosenTargets <-
    case possibleTargets of
      [] -> pure []
      _ -> Gen.list (Range.linear 0 2) (Gen.element possibleTargets)

  foreignKeys <- traverse genForeignKey chosenTargets

  pure $ table {testTableForeignKeys = foreignKeys}

generateForeignKeyAction :: HH.Gen Orville.ForeignKeyAction
generateForeignKeyAction =
  Gen.element
    [ Orville.NoAction
    , Orville.Restrict
    , Orville.Cascade
    , Orville.SetNull
    , Orville.SetDefault
    ]

generateTestIndexes :: [String] -> String -> HH.Gen [TestIndex]
generateTestIndexes columns tableName = do
  testIndices <- fmap Maybe.catMaybes $
    Gen.list (Range.linear 0 10) $ do
      -- The use of `take 8` is to avoid creating a prefix that would be truncated
      -- but is also long enough to avoid collision when generating indexes for
      -- an arbitrary amount of tables
      indexName <- Gen.maybe $ PgGen.pgIdentifierWithPrefix ((take 8 tableName) <> "i_") 3
      subcolumns <- Gen.subsequence columns
      maybeNonEmptyColumns <- NEL.nonEmpty <$> Gen.shuffle subcolumns
      uniqueness <- Gen.element [Orville.UniqueIndex, Orville.NonUniqueIndex]
      strategy <-
        Gen.frequency
          [ (10, pure Orville.Transactional)
          , (1, pure Orville.Concurrent)
          ]
      pure $ fmap (TestIndex indexName uniqueness strategy) maybeNonEmptyColumns

  pure $ (List.nubBy (Function.on (==) testIndexColumns)) testIndices

generateTestUniqueConstraints :: [String] -> HH.Gen [NEL.NonEmpty String]
generateTestUniqueConstraints columns =
  fmap Maybe.catMaybes $
    Gen.list
      (Range.linear 0 5)
      (NEL.nonEmpty <$> Gen.subsequence columns)

testTableSchemaItem :: TestTable -> AutoMigration.SchemaItem
testTableSchemaItem testTable =
  let
    addTableItems ::
      Orville.TableDefinition key writeEntity readEntity ->
      Orville.TableDefinition key writeEntity readEntity
    addTableItems tableDef =
      Orville.addTableConstraints (testTableForeignKeyDefinition <$> testTableForeignKeys testTable)
        . Orville.addTableConstraints (mkUniqueConstraint <$> testTableUniqueConstraints testTable)
        . Orville.addTableIndexes (mkIndexDefinition <$> testTableIndexes testTable)
        . Orville.setTableSchema "orville_migration_test"
        $ tableDef
  in
    case testTablePrimaryKeyDefinition testTable of
      Nothing ->
        AutoMigration.SchemaTable $
          addTableItems $
            Orville.mkTableDefinitionWithoutKey
              (testTableName testTable)
              (intColumnsMarshaller $ testTableColumns testTable)
      Just primaryKey ->
        AutoMigration.SchemaTable $
          addTableItems $
            Orville.mkTableDefinition
              (testTableName testTable)
              primaryKey
              (intColumnsMarshaller $ testTableColumns testTable)

testTablePrimaryKeyDefinition :: TestTable -> Maybe (Orville.PrimaryKey [Int32])
testTablePrimaryKeyDefinition testTable =
  let
    mkPart (index, column) =
      Orville.primaryKeyPart (!! index) (Orville.integerField column)
  in
    case zip [1 ..] (testTablePrimaryKey testTable) of
      [] ->
        Nothing
      (first : rest) ->
        Just $
          Orville.compositePrimaryKey
            (mkPart first)
            (fmap mkPart rest)

testTableForeignKeyDefinition :: TestForeignKey -> Orville.ConstraintDefinition
testTableForeignKeyDefinition foreignKey =
  let
    mkForeignReference (localColumn, foreignColumn) =
      Orville.foreignReference
        (Orville.stringToFieldName localColumn)
        (Orville.stringToFieldName foreignColumn)

    foreignTableId =
      Orville.setTableIdSchema "orville_migration_test"
        . Orville.unqualifiedNameToTableId
        . testForeignKeyTableName
        $ foreignKey
  in
    Orville.foreignKeyConstraintWithOptions
      foreignTableId
      (fmap mkForeignReference $ testForeignKeyReferences foreignKey)
      ( Orville.defaultForeignKeyOptions
          { Orville.foreignKeyOptionsOnUpdate = testForeignKeyOnUpdate foreignKey
          , Orville.foreignKeyOptionsOnDelete = testForeignKeyOnDelete foreignKey
          }
      )

generateTestTableColumns :: HH.Gen [String]
generateTestTableColumns =
  List.nub <$> Gen.list (Range.constant 0 10) PgGen.pgIdentifier

mkIntListTable :: String -> [String] -> Orville.TableDefinition Orville.NoKey [Int32] [Int32]
mkIntListTable tableName =
  mkIntListTableWithComments tableName . fmap (\c -> (c, Nothing))

mkIntListTableWithComments :: String -> [(String, Maybe String)] -> Orville.TableDefinition Orville.NoKey [Int32] [Int32]
mkIntListTableWithComments tableName columnsAndComments =
  Orville.mkTableDefinitionWithoutKey tableName (intColumnsMarshallerWithComments columnsAndComments)

intColumnsMarshaller :: [String] -> Orville.SqlMarshaller [Int32] [Int32]
intColumnsMarshaller = intColumnsMarshallerWithComments . fmap (\c -> (c, Nothing))

intColumnsMarshallerWithComments :: [(String, Maybe String)] -> Orville.SqlMarshaller [Int32] [Int32]
intColumnsMarshallerWithComments columns =
  let
    field (idx, (column, mbComment)) =
      Orville.marshallField (!! idx)
        . maybe id Orville.setFieldDescription mbComment
        $ Orville.integerField column
  in
    traverse field (zip [0 ..] columns)

mkUniqueConstraint :: NEL.NonEmpty String -> Orville.ConstraintDefinition
mkUniqueConstraint columnList =
  Orville.uniqueConstraint (fmap Orville.stringToFieldName columnList)

mkNamedUniqueConstraint :: String -> NEL.NonEmpty String -> Orville.ConstraintDefinition
mkNamedUniqueConstraint constrName columnList =
  Orville.namedConstraint
    (Schema.unqualifiedNameToConstraintId constrName)
    (RawSql.unsafeFromRawSql . RawSql.toRawSql $ Expr.uniqueConstraint . fmap (Orville.fieldNameToColumnName . Orville.stringToFieldName) $ columnList)

mkCheckConstraint :: String -> Orville.ConstraintDefinition
mkCheckConstraint constrName =
  Orville.checkConstraint (Schema.unqualifiedNameToConstraintId constrName) (RawSql.fromString "col1 > 2")

mkForeignKeyConstraint :: String -> PgAssert.ForeignKeyInfo -> Orville.ConstraintDefinition
mkForeignKeyConstraint foreignTableName foreignKeyInfo =
  let
    mkForeignReference (localColumn, foreignColumn) =
      Orville.foreignReference
        (Orville.stringToFieldName localColumn)
        (Orville.stringToFieldName foreignColumn)
  in
    Orville.foreignKeyConstraintWithOptions
      (Orville.unqualifiedNameToTableId foreignTableName)
      (fmap mkForeignReference $ PgAssert.foreignKeyInfoReferences foreignKeyInfo)
      ( Orville.defaultForeignKeyOptions
          { Orville.foreignKeyOptionsOnUpdate = PgAssert.foreignKeyInfoOnUpdate foreignKeyInfo
          , Orville.foreignKeyOptionsOnDelete = PgAssert.foreignKeyInfoOnDelete foreignKeyInfo
          }
      )

migrationPlanStepStrings :: AutoMigration.MigrationPlan -> [B8.ByteString]
migrationPlanStepStrings =
  fmap RawSql.toExampleBytes . AutoMigration.migrationPlanSteps
