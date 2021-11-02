{-# LANGUAGE GADTs #-}

module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Data.ByteString.Char8 as B8
import qualified Data.Foldable as Fold
import Data.Int (Int32)
import Data.List ((\\))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDefinition
import qualified Orville.PostgreSQL.PgCatalog as PgCatalog

import qualified Test.Entities.Foo as Foo
import qualified Test.PgAssert as PgAssert
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

autoMigrationTests :: Pool.Pool Conn.Connection -> Property.Group
autoMigrationTests pool =
  Property.group
    "AutoMigration"
    [ prop_createsMissingTables pool
    , prop_addsAndRemovesColumns pool
    , prop_columnsWithSystemNameConflictsRaiseError pool
    , prop_altersColumnDataType pool
    , prop_addAndRemovesUniqueConstraints pool
    , prop_addAndRemovesForeignKeyConstraints pool
    ]

prop_createsMissingTables :: Property.NamedDBProperty
prop_createsMissingTables =
  Property.singletonNamedDBProperty "Creates missing tables" $ \pool -> do
    firstTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql Foo.table
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    secondTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    length (firstTimeSteps) === 1
    map RawSql.toBytes secondTimeSteps === []

prop_addsAndRemovesColumns :: Property.NamedDBProperty
prop_addsAndRemovesColumns =
  Property.namedDBProperty "Adds and removes columns columns" $ \pool -> do
    let genColumnList =
          Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    originalColumns <- HH.forAll genColumnList
    newColumns <- HH.forAll genColumnList

    let columnsToDrop =
          originalColumns \\ newColumns

        originalTableDef =
          mkIntListTable "migration_test" originalColumns

        newTableDef =
          Orville.dropColumns columnsToDrop $
            mkIntListTable "migration_test" newColumns

    firstTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid $ TableDefinition.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    secondTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    PgAssert.assertColumnNamesEqual tableDesc newColumns

prop_columnsWithSystemNameConflictsRaiseError :: Property.NamedDBProperty
prop_columnsWithSystemNameConflictsRaiseError =
  Property.singletonNamedDBProperty "An error is raised trying to add a column that conflicts with a system name" $ \pool -> do
    let tableWithSystemAttributeNames =
          Orville.mkTableDefinitionWithoutKey
            "table_with_system_attribute_names"
            (Orville.marshallField id (Orville.unboundedTextField "tableoid"))

    result <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql tableWithSystemAttributeNames

          -- Create the table with no columns first to ensure we go down the
          -- "add column" path
          Orville.executeVoid $
            Expr.createTableExpr
              (Orville.tableName tableWithSystemAttributeNames)
              []
              Nothing
              []

          ExSafe.try $ AutoMigration.autoMigrateSchema [AutoMigration.schemaTable tableWithSystemAttributeNames]

    case result of
      Left err ->
        Conn.sqlExecutionErrorSqlState err === Just (B8.pack "42701")
      Right () -> do
        HH.annotate "Expected migration to fail, but it did not"
        HH.failure

data SomeField where
  SomeField :: Orville.FieldDefinition nullability a -> SomeField

prop_altersColumnDataType :: Property.NamedDBProperty
prop_altersColumnDataType =
  Property.namedDBProperty "Alters data type on existing column" $ \pool -> do
    let baseFieldDefs =
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

        describeField (SomeField field) =
          B8.unpack (RawSql.toBytes $ Orville.fieldColumnDefinition field)

    SomeField originalField <- HH.forAllWith describeField generateFieldDefinition
    SomeField newField <- HH.forAllWith describeField generateFieldDefinition

    let originalTableDef =
          Orville.mkTableDefinitionWithoutKey
            "migration_test"
            (Orville.marshallField id originalField)

        newTableDef =
          Orville.mkTableDefinitionWithoutKey
            "migration_test"
            (Orville.marshallField id newField)

        newSqlType =
          Orville.fieldType newField

    firstTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid $ TableDefinition.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    secondTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    attr <- PgAssert.assertColumnExists tableDesc "column"
    PgCatalog.pgAttributeTypeOid attr === Orville.sqlTypeOid newSqlType
    PgCatalog.pgAttributeMaxLength attr === Orville.sqlTypeMaximumLength newSqlType
    PgCatalog.pgAttributeIsNotNull attr === Orville.fieldIsNotNullable newField

prop_addAndRemovesUniqueConstraints :: Property.NamedDBProperty
prop_addAndRemovesUniqueConstraints =
  Property.namedDBProperty "Adds and removes unique constraints" $ \pool -> do
    let genColumnList =
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

    let columnsToDrop =
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

    firstTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema [AutoMigration.schemaTable originalTableDef]
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (map RawSql.toBytes firstTimeSteps))

    secondTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"

    Fold.traverse_ (PgAssert.assertUniqueConstraintExists tableDesc) newConstraintColumns
    length (PgCatalog.relationConstraints tableDesc) === length (List.nub newConstraintColumns)

prop_addAndRemovesForeignKeyConstraints :: Property.NamedDBProperty
prop_addAndRemovesForeignKeyConstraints =
  Property.namedDBProperty "Adds and removes foreign key constraints" $ \pool -> do
    let genColumnList =
          Gen.subsequence ["foo", "bar", "baz", "bat", "bax"]

    localColumns <- HH.forAll genColumnList
    foreignColumns <- HH.forAll genColumnList

    let genConstraintColumns :: HH.Gen [(NEL.NonEmpty (String, String))]
        genConstraintColumns =
          fmap Maybe.catMaybes $
            Gen.list (Range.linear 0 10) $ do
              shuffledLocal <- Gen.shuffle localColumns
              shuffledForeign <- Gen.shuffle foreignColumns

              NEL.nonEmpty <$> Gen.subsequence (zip shuffledLocal shuffledForeign)

    originalConstraintColumns <- HH.forAll genConstraintColumns
    newConstraintColumns <- HH.forAll genConstraintColumns

    -- We sort the columns in the unique constraints here to avoid edge cases
    -- with equivalent unique constraints in a different order. In these
    -- situations PostgreSQL can end up creating foreign keys that depending on
    -- indexes with a different ordering than the foreign key, which this test
    -- would then assume can be dropped even though it cannot. Standardizing
    -- the order of the columns in the unique constraints ensures this test
    -- will not try to drop a constraint that is used in both the original and
    -- new schemas while a foreign key is dropped and a new one added.
    let originalUniqueConstraints =
          fmap (mkUniqueConstraint . NEL.sort . fmap snd) originalConstraintColumns

        newUniqueConstraints =
          fmap (mkUniqueConstraint . NEL.sort . fmap snd) newConstraintColumns

        originalForeignKeyConstraints =
          fmap (mkForeignKeyConstraint "migration_test_foreign") originalConstraintColumns

        newForeignKeyConstraints =
          fmap (mkForeignKeyConstraint "migration_test_foreign") newConstraintColumns

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
          [ AutoMigration.schemaTable originalForeignTableDef
          , AutoMigration.schemaTable originalLocalTableDef
          ]

    newSchema <-
      HH.forAllWith (show . map AutoMigration.schemaItemSummary) $
        Gen.shuffle
          [ AutoMigration.schemaTable newForeignTableDef
          , AutoMigration.schemaTable newLocalTableDef
          ]

    HH.cover 5 (String.fromString "Adding Constraints") (not $ null (newConstraintColumns \\ originalConstraintColumns))
    HH.cover 5 (String.fromString "Dropping Constraints") (not $ null (originalConstraintColumns \\ newConstraintColumns))

    firstTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalLocalTableDef
          Orville.executeVoid $ TestTable.dropTableDefSql originalForeignTableDef
          AutoMigration.autoMigrateSchema originalSchema
          AutoMigration.generateMigrationSteps newSchema

    HH.annotate ("First time migration steps: " <> show (map RawSql.toBytes firstTimeSteps))

    secondTimeSteps <-
      HH.evalIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps newSchema

    HH.annotate ("Second time migration steps: " <> show (map RawSql.toBytes secondTimeSteps))

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    Fold.traverse_ (PgAssert.assertForeignKeyConstraintExists tableDesc) newConstraintColumns
    length (PgCatalog.relationConstraints tableDesc) === length (List.nub newConstraintColumns)

mkIntListTable :: String -> [String] -> Orville.TableDefinition Orville.NoKey [Int32] [Int32]
mkIntListTable tableName columns =
  Orville.mkTableDefinitionWithoutKey tableName (intColumnsMarshaller columns)

intColumnsMarshaller :: [String] -> Orville.SqlMarshaller [Int32] [Int32]
intColumnsMarshaller columns =
  let field (idx, column) =
        Orville.marshallField (!! idx) $ Orville.integerField column
   in traverse field (zip [1 ..] columns)

mkUniqueConstraint :: NEL.NonEmpty String -> Orville.ConstraintDefinition
mkUniqueConstraint columnList =
  Orville.uniqueConstraint (fmap Orville.stringToFieldName columnList)

mkForeignKeyConstraint :: String -> NEL.NonEmpty (String, String) -> Orville.ConstraintDefinition
mkForeignKeyConstraint foreignTableName referenceList =
  let mkForeignReference (localColumn, foreignColumn) =
        Orville.foreignReference
          (Orville.stringToFieldName localColumn)
          (Orville.stringToFieldName foreignColumn)
   in Orville.foreignKeyConstraint
        (Orville.unqualifiedNameToTableId foreignTableName)
        (fmap mkForeignReference referenceList)
