module Test.AutoMigration
  ( autoMigrationTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

autoMigrationTests :: Pool.Pool Conn.Connection -> IO Bool
autoMigrationTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "AutoMigration")
      [
        ( String.fromString "Creates missing tables"
        , Property.singletonProperty $ do
            firstTimeSteps <-
              MIO.liftIO $
                Orville.runOrville pool $ do
                  Orville.withConnection $ \conn ->
                    MIO.liftIO (TestTable.dropTableDef conn Foo.table)

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
