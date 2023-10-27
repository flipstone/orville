#!/usr/bin/env mdsh

# Adding a new non-nullable column

We'll show how, to add a new non-nullable column:
1. first, we create the initial version of the table without the 'new' column
1. we then add the column, but only as *nullable*
1. we make sure all values are present on the nullable column
1. we then migrate the column to be non-nullable

First let's initialize a Haskell project. This is all explained in previous
tutorials.

```shell
mkdir orville-migration
cd orville-migration
cabal init -n --exe
sed -i -re 's/build-depends:/build-depends: orville-postgresql, text,/' *.cabal
cat << 'EOF' > cabal.project
packages: .
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: c3bdcebac4beb8ef50715439ea24562ed2b95b36
  subdir: orville-postgresql
EOF
cat << 'EOF' > app/Main.hs
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
EOF
```

Note how the following tables have the same SQL table names. Imagine that
`table1` is the initial version of the table, which then is changed to
`table2`, and so on. In practice, they can keep their Haskell names, since they
won't need to co-exist, like they do in this document.

Orville's AutoMigration tool sees that the difference between the tables is,
that the second table has an additional column, and it will generate and
execute the DDL to add the column. It needs to be nullable, because the database won't
have any values for it when it is added.

```shell
cat << 'EOF' >> app/Main.hs
table1 :: O.TableDefinition (O.HasKey Int.Int32) Foo1 Foo1
table1 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo1Marshaller

table2 :: O.TableDefinition (O.HasKey Int.Int32) Foo2 Foo2
table2 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo2Marshaller

table3 :: O.TableDefinition (O.HasKey Int.Int32) Foo3 Foo3
table3 =
  O.mkTableDefinition "migration_demo1" (O.primaryKey fooIdField) foo3Marshaller
EOF
```

```shell
cat << 'EOF' >> app/Main.hs
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

  O.runOrville pool $ do
    O.executeVoid O.DDLQuery (RawSql.fromString "DROP TABLE IF EXISTS migration_demo1")
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table1 ]
    _ <- O.insertEntity table1 Foo1 { foo1Id = 0 }
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table2 ]
    _ <- O.updateEntity table2 0 Foo2 { foo2Id = 0, foo2Age = Just 91 }
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable table3 ]
    liftIO . print =<< O.findEntity table3 0
EOF
```

# Dropping a column

Orville won't automatically drop a column in the SQL database that isn't in the SqlMarshaller.
The column will just be ignored.

We have to tell Orville explicitly about the columns that are safe to drop.
This is done using the `dropColumns` combinator:

```shell
cat << 'EOF' >> app/Main.hs
    AutoMigration.autoMigrateSchema AutoMigration.defaultOptions [ AutoMigration.SchemaTable $ O.dropColumns ["age"] table1 ]
    liftIO . print =<< O.findEntity table1 0
EOF
```

# Conclusion

This concludes this tutorial. The expected output is visible just above the EOF:

```shell
cabal build
cat << 'EOF' > migration-test.t
$ cd $TESTDIR
$ cp $(cabal list-bin exe:orville-migration | tail -n1) $OLDPWD
$ cd $OLDPWD
$ ./orville-migration
Just (Foo3 {foo3Id = 0, foo3Age = 91})
Just (Foo1 {foo1Id = 0})
EOF
~/.local/bin/prysk migration-test.t --indent=0
```
