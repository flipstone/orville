<img
    src="images/orville-waving-pennant.svg"
    width="250px"
    height="250px"
    align="left">

# Orville - a Haskell library for PostgreSQL

Orville's goal is to provide a powerful API for applications to access
PostgreSQL databases with minimal use of sophisticated language techniques or
extensions. It strikes a balance between enforcing type-safety in database
interactions where it is reasonable and presenting type signatures that are
minimally complicated.

<br clear="left"/>

## Why Orville?

Orville is not meant to replace existing PostgreSQL libraries in the Haskell
ecosystem, but to complement them. It has the power to satisfy most experienced
Haskell developers but strives to remain approachable to newcomers despite
this. Orville's API is rich enough to be used in production on large and
sophisticated applications, but avoids complicated type-level programming. If
your application is too large to reasonably write all your SQL statements by
hand yet doesn't require absolute type-safety between your custom SQL
statements, their result sets and the Haskell types they decode into, Orville
may be the right choice for you.

## Feature Overview

* Rich API for marshalling Haskell types to and from SQL
* High-level APIs for common CRUD operations
* Optional automatic schema migrations
* Optional API for executing complex data loads across multiple tables without ever writing an N+1 query by accident
* Progressive escape hatches to let you dig deeper when you need to

## Tutorials

See the tutorials, in order of increasing complexity:

* [Getting Started](../GETTING-STARTED.md)
* [Using SqlMarshaller](../SQL-MARSHALLER.md)
* [Using Plans](../PLAN.md)
* [Using Migrations](../MIGRATION.md)
* [Using JSON](../JSON.md)

Additional documentation is available in the Haddocks.

## Just show me some code!

Ok! Here's a very simple application that inserts some entities of a `Pet`
model and finds one of them based on its name.

```haskell
module Main (main) where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

{- |
  Pet is a plain old Haskell record that will be marshalled to and from the
  @pet@ table.
-}
data Pet =
  Pet
    { petId :: PetId
    , petName :: T.Text
    }

{- |
  It's good practice to create a newtype specific to each entity to hold its
  primary key value.
-}
newtype PetId = PetId Int32

{- |
  A marshaller must be defined to convert Pet to and from SQL.
-}
petMarshaller :: O.SqlMarshaller Pet Pet
petMarshaller =
  Pet
    <$> O.marshallField petId petIdField
    <*> O.marshallField petName nameField

{- |
  Defines the @id@ field for the marshaller to marshall the 'petId' record
  field to and from.
-}
petIdField :: O.FieldDefinition O.NotNull PetId
petIdField =
  O.coerceField (O.integerField "id")

{- |
  Defines the @name@ field for the marshaller to marshall the 'petName' record
  field to and from.
-}
nameField :: O.FieldDefinition O.NotNull T.Text
nameField =
  O.unboundedTextField "name"

{- |
  Marshaller above is associated with the @pet@ table. The marshaller's fields
  will define the columns of the table.
-}
petTable :: O.TableDefinition (O.HasKey PetId) Pet Pet
petTable =
  O.mkTableDefinition
    "pet"
    (O.primaryKey petIdField)
    petMarshaller

{- |
  A simple demo that connects to a database, inserts 2 pets and then finds the
  pet named "Spot"
-}
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
      O.ConnectionOptions
        { O.connectionString = "host=localhost user=orville password=orville"
        , O.connectionNoticeReporting = O.DisableNoticeReporting
        , O.connectionPoolStripes = O.OneStripePerCapability
        , O.connectionPoolMaxConnectionsPerStripe = 1
        , O.connectionPoolLingerTime = 10
        }

  mbSpot <- O.runOrville pool insertAndFindSpot

  case mbSpot of
    Nothing -> putStrLn "No Spot Found!"
    Just _spot -> putStrLn "Spot found!"

{- |
  The Orville monad provides a starter pack for running Orville operations
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
```
