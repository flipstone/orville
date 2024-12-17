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
import Data.Int (Int32)
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema as Schema

import qualified Test.Entities.Foo as Foo
import Test.Orphans ()
import qualified Test.PgAssert as PgAssert
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

autoMigrationTests :: Orville.ConnectionPool -> Property.Group
autoMigrationTests pool =
  Property.group
    "AutoMigration"
    [ prop_altersColumnAddIdentity pool
    , prop_raisesErrorIfMigrationLockIsLocked pool
    , prop_releasesMigrationLockOnError pool
    , prop_createsMissingTables pool
    , prop_dropsRequestedTables pool
    , prop_addsAndRemovesColumns pool
    , prop_columnsWithSystemNameConflictsRaiseError pool
    , prop_altersColumnDataType pool
    , prop_altersColumnDefaultValue_TextNumeric pool
    , prop_altersColumnDefaultValue_Bool pool
    , prop_altersColumnDefaultValue_Timelike pool
    , prop_respectsImplicitDefaultOnSerialFields pool
    , prop_addAndRemovesUniqueConstraints pool
    , prop_addAndRemovesForeignKeyConstraints pool
    , prop_createsMissingSequences pool
    , prop_dropsRequestedSequences pool
    , prop_altersModifiedSequences pool
    , prop_addsAndRemovesMixedIndexes pool
    , prop_arbitrarySchemaInitialMigration pool
    , prop_createsMissingFunctions pool
    , prop_recreatesAlteredFunctions pool
    , prop_dropsRequestedFunctions pool
    , prop_createsMissingTriggers pool
    , prop_dropsUnrequestedTriggers pool
    , prop_loadsMissingExtensions pool
    , prop_unloadsPresentExtensions pool
    , prop_addsTableComment pool
    , prop_removesTableComment pool
    , prop_modifiesTableComment pool
    , prop_addsColumnCommmentsOnCreateTable pool
    , prop_modifiesColumnComments pool
    ]

prop_raisesErrorIfMigrationLockIsLocked :: Property.NamedDBProperty
prop_raisesErrorIfMigrationLockIsLocked =
  Property.singletonNamedDBProperty "Raises an error when the migration lock is hold" $ \pool -> do
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

prop_releasesMigrationLockOnError :: Property.NamedDBProperty
prop_releasesMigrationLockOnError =
  Property.singletonNamedDBProperty "Releases the migration lock on error" $ \pool -> do
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

prop_createsMissingTables :: Property.NamedDBProperty
prop_createsMissingTables =
  Property.singletonNamedDBProperty "Creates missing tables" $ \pool -> do
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

prop_dropsRequestedTables :: Property.NamedDBProperty
prop_dropsRequestedTables =
  Property.singletonNamedDBProperty "Drops requested tables" $ \pool -> do
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

prop_addsTableComment :: Property.NamedDBProperty
prop_addsTableComment =
  Property.singletonNamedDBProperty "Adds a comment to a table" $ \pool -> do
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

prop_removesTableComment :: Property.NamedDBProperty
prop_removesTableComment =
  Property.singletonNamedDBProperty "Removes a comment from a table" $ \pool -> do
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

prop_modifiesTableComment :: Property.NamedDBProperty
prop_modifiesTableComment =
  Property.singletonNamedDBProperty "Modifies the comment on a table" $ \pool -> do
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

prop_addsColumnCommmentsOnCreateTable :: Property.NamedDBProperty
prop_addsColumnCommmentsOnCreateTable =
  Property.singletonNamedDBProperty "Adds column comments when creating table" $ \pool -> do
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

prop_modifiesColumnComments :: Property.NamedDBProperty
prop_modifiesColumnComments =
  Property.singletonNamedDBProperty "Modifies column comments" $ \pool -> do
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

prop_addsAndRemovesColumns :: Property.NamedDBProperty
prop_addsAndRemovesColumns =
  Property.namedDBProperty "Adds and removes columns" $ \pool -> do
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

prop_columnsWithSystemNameConflictsRaiseError :: Property.NamedDBProperty
prop_columnsWithSystemNameConflictsRaiseError =
  Property.singletonNamedDBProperty "An error is raised trying to add a column that conflicts with a system name" $ \pool -> do
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

{- |
  Migration Guide: @SomeField@ has been removed. @foldMarshallerFields@ can be
  used to collect data from the fields in a @SqlMarshaller@ while converting
  the results to whatever type you desire.

@since 1.0.0.0
-}
data SomeField where
  SomeField :: Orville.FieldDefinition nullability a -> SomeField

describeField :: SomeField -> String
describeField (SomeField field) =
  B8.unpack (RawSql.toExampleBytes $ Orville.fieldColumnDefinition field)

prop_altersColumnDataType :: Property.NamedDBProperty
prop_altersColumnDataType =
  Property.namedDBProperty "Alters data type on existing column" $ \pool -> do
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

prop_altersColumnDefaultValue_TextNumeric :: Property.NamedDBProperty
prop_altersColumnDefaultValue_TextNumeric =
  Property.namedDBProperty "Alters default value on existing column (text/numeric)" $ \pool -> do
    let
      genDefaultText =
        PgGen.pgText (Range.linear 0 10)

      genDefaultIntegral :: Integral n => HH.Gen n
      genDefaultIntegral =
        Gen.integral (Range.linear (-10) 10)

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

prop_altersColumnAddIdentity :: Property.NamedDBProperty
prop_altersColumnAddIdentity =
  Property.namedDBProperty "Alters an existing column to add IDENTITY" $ \pool -> do
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

prop_altersColumnDefaultValue_Bool :: Property.NamedDBProperty
prop_altersColumnDefaultValue_Bool =
  Property.namedDBProperty "Alters default value on existing column (boolean)" $ \pool -> do
    assertDefaultValuesMigrateProperly pool $
      Gen.choice
        [ SomeField <$> genFieldWithMaybeDefault Gen.bool Orville.booleanDefault (Orville.booleanField "column")
        ]

prop_altersColumnDefaultValue_Timelike :: Property.NamedDBProperty
prop_altersColumnDefaultValue_Timelike =
  Property.namedDBProperty "Alters default value on existing column (timelike)" $ \pool -> do
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

prop_respectsImplicitDefaultOnSerialFields :: Property.NamedDBProperty
prop_respectsImplicitDefaultOnSerialFields =
  Property.namedDBProperty "Respects implicit default on serial fields" $ \pool -> do
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

prop_addAndRemovesUniqueConstraints :: Property.NamedDBProperty
prop_addAndRemovesUniqueConstraints =
  Property.namedDBProperty "Adds and removes unique constraints" $ \pool -> do
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
    length (PgCatalog.relationConstraints tableDesc) === length (List.nub newConstraintColumns)

prop_addAndRemovesForeignKeyConstraints :: Property.NamedDBProperty
prop_addAndRemovesForeignKeyConstraints =
  Property.namedDBProperty "Adds and removes foreign key constraints" $ \pool -> do
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
    length (PgCatalog.relationConstraints tableDesc) === length (List.nub newForeignKeyInfos)

prop_addsAndRemovesMixedIndexes :: Property.NamedDBProperty
prop_addsAndRemovesMixedIndexes =
  Property.namedDBProperty "Adds and removes named indexes" $ \pool -> do
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

      newTableDef =
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
          Orville.executeVoid Orville.DDLQuery $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable originalTableDef]
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (migrationPlanStepStrings firstTimePlan))

    originalTableDesc <- PgAssert.assertTableExists pool "migration_test"
    Fold.traverse_
      (PgAssert.assertIndexExists originalTableDesc <$> testIndexUniqueness <*> testIndexColumns)
      originalTestIndexes

    secondTimePlan <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationPlan AutoMigration.defaultOptions firstTimePlan
          AutoMigration.generateMigrationPlan AutoMigration.defaultOptions [AutoMigration.SchemaTable newTableDef]

    migrationPlanStepStrings secondTimePlan === []
    newTableDesc <- PgAssert.assertTableExists pool "migration_test"

    Fold.traverse_
      (PgAssert.assertIndexExists newTableDesc <$> testIndexUniqueness <*> testIndexColumns)
      newTestIndexes
    length (PgCatalog.relationIndexes newTableDesc) === length (List.nub newTestIndexes)

prop_createsMissingSequences :: Property.NamedDBProperty
prop_createsMissingSequences =
  Property.singletonNamedDBProperty "Creates missing sequences" $ \pool -> do
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

prop_dropsRequestedSequences :: Property.NamedDBProperty
prop_dropsRequestedSequences =
  Property.singletonNamedDBProperty "Drops requested sequences" $ \pool -> do
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

prop_altersModifiedSequences :: Property.NamedDBProperty
prop_altersModifiedSequences =
  Property.namedDBProperty "Alters modified sequences" $ \pool -> do
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

prop_arbitrarySchemaInitialMigration :: Property.NamedDBProperty
prop_arbitrarySchemaInitialMigration =
  Property.namedDBProperty "An arbitrary list of schema items can be created from scratch" $ \pool -> do
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

prop_createsMissingFunctions :: Property.NamedDBProperty
prop_createsMissingFunctions =
  Property.singletonNamedDBProperty "Creates missing functions" $ \pool -> do
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

prop_recreatesAlteredFunctions :: Property.NamedDBProperty
prop_recreatesAlteredFunctions =
  Property.singletonNamedDBProperty "Recreates functions with altered source code" $ \pool -> do
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
            Expr.dropFunction (Just Expr.ifExists) (Orville.functionName oldFunctionDef)
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

prop_dropsRequestedFunctions :: Property.NamedDBProperty
prop_dropsRequestedFunctions =
  Property.singletonNamedDBProperty "Drops requested functions" $ \pool -> do
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

prop_createsMissingTriggers :: Property.NamedDBProperty
prop_createsMissingTriggers =
  Property.singletonNamedDBProperty "Creates missing triggers" $ \pool -> do
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
          Orville.executeVoid Orville.DDLQuery $ Expr.dropFunction (Just Expr.ifExists) (Orville.functionName functionDef)
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

prop_dropsUnrequestedTriggers :: Property.NamedDBProperty
prop_dropsUnrequestedTriggers =
  Property.singletonNamedDBProperty "Drops unrequested triggers" $ \pool -> do
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
          Orville.executeVoid Orville.DDLQuery $ Expr.dropFunction (Just Expr.ifExists) (Orville.functionName functionDef)
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

prop_loadsMissingExtensions :: Property.NamedDBProperty
prop_loadsMissingExtensions =
  Property.singletonNamedDBProperty "Loads missing extensions" $ \pool -> do
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

prop_unloadsPresentExtensions :: Property.NamedDBProperty
prop_unloadsPresentExtensions =
  Property.singletonNamedDBProperty "Unloads present extensions" $ \pool -> do
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
  deriving (Show, Eq)

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
