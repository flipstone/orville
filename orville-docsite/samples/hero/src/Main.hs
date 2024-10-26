module Main (main) where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

{- | Pet is a plain old Haskell record that will be marshalled to and from the
  @pet@ table.
-}
data Pet =
  Pet
    { petId :: PetId
    , petName :: T.Text
    }

{- | It's good practice to create newtype specific to each entity to hold its
  primary key value
-}
newtype PetId = PetId Int32

{- | A marshaller must be defined to convert Pet to and from SQL.
-}
petMarshaller :: O.SqlMarshaller Pet Pet
petMarshaller =
  Pet
    <$> O.marshallField petId petIdField
    <*> O.marshallField petName nameField

{- | Defines the @id@ field for the marshaller to marshall the 'petId' record
  field to and from.
-}
petIdField :: O.FieldDefinition O.NotNull PetId
petIdField =
  O.coerceField (O.integerField "id")

{- | Defines the @name@ field for the marshaller to marshall the 'petName' record
  field to and from.
-}
nameField :: O.FieldDefinition O.NotNull T.Text
nameField =
  O.unboundedTextField "name"

{- | Marshaller above is associated with the @pet@ table. The marshallers fields
  will define the column of the table.
-}
petTable :: O.TableDefinition (O.HasKey PetId) Pet Pet
petTable =
  O.mkTableDefinition
    "pet"
    (O.primaryKey petIdField)
    petMarshaller

{- | A simple demo that connects to a database, inserts 2 pets and then finds the
  pet named "Spot"
-}
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
      O.ConnectionOptions
        { O.connectionString = "host=localhost user=postgres password=postgres"
        , O.connectionNoticeReporting = O.DisableNoticeReporting
        , O.connectionPoolStripes = O.OneStripePerCapability
        , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
        , O.connectionPoolLingerTime = 10
        }

  mbSpot <- O.runOrville pool insertAndFindSpot

  case mbSpot of
    Nothing -> putStrLn "No Spot Found!"
    Just _spot -> putStrLn "Spot found!"

{- | The Orville monad provides a starter pack for running Orville operations
  against a connection pool.
-}
insertAndFindSpot :: O.Orville (Maybe Pet)
insertAndFindSpot = do
  AutoMigration.autoMigrateSchema
    AutoMigration.defaultOptions
    [AutoMigration.SchemaTable petTable]

  O.insertEntity petTable $
    Pet
      { petId = PetId 1
      , petName = T.pack "FuFu"
      }

  O.insertEntity petTable $
    Pet
      { petId = PetId 2
      , petName = T.pack "Spot"
      }

  O.findFirstEntityBy
    petTable
    (O.where_ (O.fieldEquals nameField (T.pack "Spot")))
