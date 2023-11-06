-- SNIPPET: moduleHeaderAndTypes
module Main
  ( main
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Int as Int

data Foo1 = Foo1
  { foo1Id :: Int.Int32
  }
  deriving Show

data Foo2 = Foo2
  { foo2Id :: Int.Int32
  , foo2Age :: Maybe Int.Int32
  }
  deriving Show

data Foo3 = Foo3
  { foo3Id :: Int.Int32
  , foo3Age :: Int.Int32
  }
  deriving Show

fooIdField :: O.FieldDefinition O.NotNull Int.Int32
fooIdField =
  O.integerField "id"

fooAgeField :: O.FieldDefinition O.NotNull Int.Int32
fooAgeField =
  O.integerField "age"

foo1Marshaller :: O.SqlMarshaller Foo1 Foo1
foo1Marshaller =
  Foo1
    <$> O.marshallField foo1Id fooIdField

foo2Marshaller :: O.SqlMarshaller Foo2 Foo2
foo2Marshaller =
  Foo2
    <$> O.marshallField foo2Id fooIdField
    <*> O.marshallField foo2Age (O.nullableField fooAgeField)

foo3Marshaller :: O.SqlMarshaller Foo3 Foo3
foo3Marshaller =
  Foo3
    <$> O.marshallField foo3Id fooIdField
    <*> O.marshallField foo3Age fooAgeField
-- SNIPPET: tableDefinitions
table1 :: O.TableDefinition (O.HasKey Int.Int32) Foo1 Foo1
table1 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo1Marshaller

table2 :: O.TableDefinition (O.HasKey Int.Int32) Foo2 Foo2
table2 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo2Marshaller

table3 :: O.TableDefinition (O.HasKey Int.Int32) Foo3 Foo3
table3 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo3Marshaller
-- SNIPPET: mainFunction
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=localhost user=postgres password=postgres"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  O.runOrville pool $ do
    O.executeVoid O.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS migration_demo1")
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table1 ]
    _ <- O.insertEntity table1 Foo1 { foo1Id = 0 }
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table2 ]
    _ <- O.updateEntity table2 0 Foo2 { foo2Id = 0, foo2Age = Just 91 }
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table3 ]
    liftIO . print =<< O.findEntity table3 0
-- SNIPPET: droppingColumns
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable $ O.dropColumns ["age"] table1 ]
    liftIO . print =<< O.findEntity table1 0
