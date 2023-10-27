#!/usr/bin/env mdsh

# Using SqlMarshaller

The SQL Marshaller helps with converting from a Haskell record to SQL and back.
This document also shows how the SQL marshaller in table definitions, which can
then be used to generate Data Definition Language (DDL) queries to create
tables in the database.

First let's initialize a Haskell project. This first part is identical to the
GETTING-STARTED guide, so we'll avoid explaining it here.

```shell
mkdir orville-sql-marshaller
cd orville-sql-marshaller
cabal init -n --exe
sed -i -re 's/build-depends:/build-depends: orville-postgresql, text,/' *.cabal
cat << EOF > cabal.project
packages: .
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: c3bdcebac4beb8ef50715439ea24562ed2b95b36
  subdir: orville-postgresql
EOF
```

Now, let's import the necessary modules. We use less 'internal' imports here
than in the GETTING-STARTED guide, because we are using a higher abstraction
level.

```shell
cat <<EOF > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration

import qualified Data.Int as Int
import qualified Data.Text as T
EOF
```

Next, let's declare some type aliases and the Haskell record itself. We'll
enable storing values of this record in the SQL database.

```shell
cat << EOF >> app/Main.hs
type FooId = Int.Int32
type FooName = T.Text
type FooAge = Int.Int32

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  , fooAge :: FooAge
  }
  deriving (Eq, Show)
EOF
```

To store the record in SQL, we need to define Orville FieldDefinitions for each
of the fields of the record. A FieldDefinition maps to a column in SQL. Also
note how the type is parameterized on whether it is nullable or not.

The strings passed in here are the actual SQL column names. FieldDefinitions
help avoiding typos, since the Haskell compiler will fail compilation if the
name of a FieldDefinition is misspelt.

```shell
cat << EOF >> app/Main.hs
fooIdField :: O.FieldDefinition O.NotNull FooId
fooIdField =
  O.integerField "id"

fooNameField :: O.FieldDefinition O.NotNull FooName
fooNameField =
  O.unboundedTextField "name"

fooAgeField :: O.FieldDefinition O.NotNull FooAge
fooAgeField =
  O.integerField "age"
EOF
```

Now that the fields are defined, we can use them, togehter with the record
field selector functions, to define the SqlMarshaller.

```shell
cat << EOF >> app/Main.hs
fooMarshaller :: O.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> O.marshallField fooId fooIdField
    <*> O.marshallField fooName fooNameField
    <*> O.marshallField fooAge fooAgeField
EOF
```

We can use the marshaller to define the Orville table definition. This binding
represents a table, but note that it doesn't necessarily exist in the SQL
database until we start using it.

```shell
cat << EOF >> app/Main.hs
table :: O.TableDefinition (O.HasKey FooId) Foo Foo
table =
  O.mkTableDefinition "foo" (O.primaryKey fooIdField) fooMarshaller
EOF
```

Now let's write the main function, which does the following:
1. auto migrates using the table defintion. It will match the Haskell record at
   the time of execution of the statement. The details of migration are out of
   scope for this article.
1. deletes the Foo with ID 0, if it exists. Necessary to allow the program to
   be repeatedly executed, as primary keys can't be duplicated.
1. inserts an example Foo object with ID 0.
1. reads it back out using its ID 0. If an entity with the given ID doesn't
   exist, we'd get a Nothing here.
1. prints the retrieved entity

```shell
cat << EOF >> app/Main.hs
main :: IO ()
main = do
  pool <-
    O.createConnectionPool
        O.ConnectionOptions
          { O.connectionString = "host=pg user=orville_docs password=orville"
          , O.connectionNoticeReporting = O.DisableNoticeReporting
          , O.connectionPoolStripes = O.OneStripePerCapability
          , O.connectionPoolLingerTime = 10
          , O.connectionPoolMaxConnections = O.MaxConnectionsPerStripe 1
          }

  mFoo <- O.runOrville pool $ do
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [AutoMigration.SchemaTable table]
    _ <- O.deleteEntity table 0
    _ <- O.insertEntity table Foo { fooId = 0, fooName = T.pack "Name", fooAge = 91 }
    O.findEntity table 0
  print mFoo
EOF
```

The program is now complete, let's compile and run!

```shell
cabal build
```

This verifies the output, the relevant part here is just the line before EOF.

```shell
cat <<EOF > sql-marshaller-test.t
$ cd \$TESTDIR
$ cp \$(cabal list-bin exe:orville-sql-marshaller | tail -n1) \$OLDPWD
$ cd \$OLDPWD
$ ./orville-sql-marshaller
Just (Foo {fooId = 0, fooName = "Name", fooAge = 91})
EOF
~/.local/bin/prysk sql-marshaller-test.t --indent=0
```
