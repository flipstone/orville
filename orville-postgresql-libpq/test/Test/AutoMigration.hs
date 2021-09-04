module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import Hedgehog ((===))

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
    [
      ( String.fromString "Creates missing tables"
      , Property.singletonProperty $ do
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
      )
    ,
      ( String.fromString "Adds missing columns"
      , Property.singletonProperty $ do
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
      )
    ]
