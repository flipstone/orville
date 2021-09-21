{-# LANGUAGE GADTs #-}

module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Pool as Pool
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
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
    , prop_addsMissingColumns pool
    , prop_columnsWithSystemNameConflictsRaiseError pool
    , prop_altersColumnDataType pool
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

prop_addsMissingColumns :: Property.NamedDBProperty
prop_addsMissingColumns =
  Property.singletonNamedDBProperty "Adds missing columns" $ \pool -> do
    firstTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          Orville.executeVoid $ TestTable.dropTableDefSql Foo.table
          Orville.executeVoid $
            Expr.createTableExpr
              (Orville.tableName Foo.table)
              [Orville.fieldColumnDefinition Foo.fooIdField]
              (Just $ PrimaryKey.mkPrimaryKeyExpr $ Orville.tablePrimaryKey Foo.table)

          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    secondTimeSteps <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          AutoMigration.executeMigrationSteps firstTimeSteps
          AutoMigration.generateMigrationSteps [AutoMigration.schemaTable Foo.table]

    length (firstTimeSteps) === 1
    map RawSql.toBytes secondTimeSteps === []
    PgAssert.assertColumnNamesEqual
      pool
      (Orville.unqualifiedTableNameString Foo.table)
      [ Orville.fieldNameToString $ Orville.fieldName Foo.fooIdField
      , Orville.fieldNameToString $ Orville.fieldName Foo.fooNameField
      , Orville.fieldNameToString $ Orville.fieldName Foo.fooAgeField
      ]

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
    attr <- PgAssert.assertColumnExists pool "migration_test" "column"
    PgCatalog.pgAttributeTypeOid attr === Orville.sqlTypeOid newSqlType
    PgCatalog.pgAttributeMaxLength attr === Orville.sqlTypeMaximumLength newSqlType
    PgCatalog.pgAttributeIsNotNull attr === Orville.fieldIsNotNull newField
