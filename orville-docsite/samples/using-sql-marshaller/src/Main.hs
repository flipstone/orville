-- SNIPPET: moduleHeader
module Main
  ( main
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

import qualified Data.Int as Int
import qualified Data.Text as T
-- SNIPPET: dataTypes
type FooId = Int.Int32
type FooName = T.Text
type FooAge = Int.Int32

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  , fooAge :: FooAge
  }
  deriving (Eq, Show)
-- SNIPPET: fieldDefinitions
fooIdField :: O.FieldDefinition O.NotNull FooId
fooIdField =
  O.integerField "id"

fooNameField :: O.FieldDefinition O.NotNull FooName
fooNameField =
  O.unboundedTextField "name"

fooAgeField :: O.FieldDefinition O.NotNull FooAge
fooAgeField =
  O.integerField "age"
-- SNIPPET: sqlMarshaller
fooMarshaller :: O.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> O.marshallField fooId fooIdField
    <*> O.marshallField fooName fooNameField
    <*> O.marshallField fooAge fooAgeField
-- SNIPPET: tableDefinition
table :: O.TableDefinition (O.HasKey FooId) Foo Foo
table =
  O.mkTableDefinition "foo" (O.primaryKey fooIdField) fooMarshaller
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

  mbFoo <- O.runOrville pool $ do
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable table]
    _ <- O.deleteEntity table 0
    _ <- O.insertEntity table Foo { fooId = 0, fooName = T.pack "Name", fooAge = 91 }
    O.findEntity table 0
  print mbFoo
