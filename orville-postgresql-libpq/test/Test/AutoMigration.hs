{-# LANGUAGE GADTs #-}

module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
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
    ]

prop_createsMissingTables :: Property.NamedDBProperty
prop_createsMissingTables =
  Property.singletonNamedDBProperty "Creates missing tables" $ \pool -> do
    firstTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql Foo.table
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    secondTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    length (firstTimeSteps) === 1
    map RawSql.toBytes secondTimeSteps === []

prop_addsAndRemovesColumns :: Property.NamedDBProperty
prop_addsAndRemovesColumns =
  Property.namedDBProperty "Adds and removes columns columns" $ \pool -> do
    let possibleColumns =
          ["foo", "bar", "baz", "bat", "bax"]

    originalColumns <- HH.forAll $ Gen.subsequence possibleColumns
    newColumns <- HH.forAll $ Gen.subsequence possibleColumns

    let columnsToDrop =
          originalColumns \\ newColumns

        originalTableDef =
          Orville.mkTableDefinitionWithoutKey
            "migration_test"
            (intColumnsMarshaller originalColumns)

        newTableDef =
          Orville.dropColumns columnsToDrop $
            Orville.mkTableDefinitionWithoutKey
              "migration_test"
              (intColumnsMarshaller newColumns)

    firstTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid $ TableDefinition.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    secondTimeSteps <-
      MIO.liftIO $
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
      MIO.liftIO $
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
      MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          Orville.executeVoid $ TableDefinition.mkCreateTableExpr originalTableDef
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    secondTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"
    attr <- PgAssert.assertColumnExists tableDesc "column"
    PgCatalog.pgAttributeTypeOid attr === Orville.sqlTypeOid newSqlType
    PgCatalog.pgAttributeMaxLength attr === Orville.sqlTypeMaximumLength newSqlType
    PgCatalog.pgAttributeIsNotNull attr === Orville.fieldIsNotNull newField

prop_addAndRemovesUniqueConstraints :: Property.NamedDBProperty
prop_addAndRemovesUniqueConstraints =
  Property.namedDBProperty "Adds and removes unique constraints" $ \pool -> do
    let possibleColumns =
          ["foo", "bar", "baz", "bat", "bax"]

        mkUniqueConstraint columnList =
          Orville.uniqueConstraint (fmap Orville.stringToFieldName columnList)

        genConstraintColumns :: [String] -> HH.Gen (Maybe (NEL.NonEmpty String))
        genConstraintColumns columns = do
          subcolumns <- Gen.subsequence columns
          NEL.nonEmpty <$> Gen.shuffle subcolumns

    originalColumns <- HH.forAll $ Gen.subsequence possibleColumns
    originalConstraintColumns <-
      fmap Maybe.catMaybes . HH.forAll $
        Gen.list (Range.linear 0 10) (genConstraintColumns originalColumns)

    newColumns <- HH.forAll $ Gen.subsequence possibleColumns
    newConstraintColumns <-
      fmap Maybe.catMaybes . HH.forAll $
        Gen.list (Range.linear 0 10) (genConstraintColumns newColumns)

    let columnsToDrop =
          originalColumns \\ newColumns

        originalConstraints =
          fmap mkUniqueConstraint originalConstraintColumns

        newConstraints =
          fmap mkUniqueConstraint newConstraintColumns

        originalTableDef =
          Orville.addTableConstraints originalConstraints $
            Orville.mkTableDefinitionWithoutKey
              "migration_test"
              (intColumnsMarshaller originalColumns)

        newTableDef =
          Orville.addTableConstraints newConstraints $
            Orville.dropColumns columnsToDrop $
              Orville.mkTableDefinitionWithoutKey
                "migration_test"
                (intColumnsMarshaller newColumns)

    HH.cover 5 (String.fromString "Adding Constraints") (not $ null (newConstraintColumns \\ originalConstraintColumns))
    HH.cover 5 (String.fromString "Dropping Constraints") (not $ null (originalConstraintColumns \\ newConstraintColumns))

    firstTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql originalTableDef
          AutoMigration.autoMigrateSchema [AutoMigration.schemaTable originalTableDef]
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    HH.annotate ("First time migration steps: " <> show (map RawSql.toBytes firstTimeSteps))

    secondTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable newTableDef]

    map RawSql.toBytes secondTimeSteps === []
    tableDesc <- PgAssert.assertTableExists pool "migration_test"

    Fold.traverse_ (PgAssert.assertUniqueConstraintExists tableDesc) newConstraintColumns
    length (PgCatalog.relationConstraints tableDesc) === length (List.nub newConstraintColumns)

intColumnsMarshaller :: [String] -> Orville.SqlMarshaller [Int32] [Int32]
intColumnsMarshaller columns =
  let field (idx, column) =
        Orville.marshallField (!! idx) $ Orville.integerField column
   in traverse field (zip [1 ..] columns)
