module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

autoMigrationTests :: Pool.Pool Conn.Connection -> Property.Group
autoMigrationTests pool =
  Property.group
    "AutoMigration"
    [ prop_createsMissingTables pool
    , prop_addsMissingColumns pool
    , prop_columnsWithSystemNameConflictsRaiseError pool
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

prop_columnsWithSystemNameConflictsRaiseError :: Property.NamedDBProperty
prop_columnsWithSystemNameConflictsRaiseError =
  Property.singletonNamedDBProperty "An error is raised trying to add a column that conflicts with a system name" $ \pool -> do
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

tableWithSystemAttributeNames :: Orville.TableDefinition Orville.NoKey T.Text T.Text
tableWithSystemAttributeNames =
  Orville.mkTableDefinitionWithoutKey
    "table_with_system_attribute_names"
    (Orville.marshallField id (Orville.unboundedTextField "tableoid"))
