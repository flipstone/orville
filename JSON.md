#!/usr/bin/env mdsh

# Using JSON

SQL has a rigid schema. Using JSON inside SQL allows for pockets of schemaless
data, that is still queryable using PostgreSQL's built-in functionality.

This document explains how to use Orville with the JSONB data type that
PostgreSQL natively supports. The data type isn't built in to Orville, but it's
shown here how to add it.

Project initialization is similar to previous tutorials, but additional
dependencies like Aeson have been added. Aeson is a JSON library for Haskell.

```shell
mkdir orville-json
cd orville-json
cabal init -n --exe
sed -i -re 's/build-depends:/build-depends: orville-postgresql-libpq, aeson, bytestring, postgresql-libpq, resource-pool, text, vector,/' *.cabal
cat << 'EOF' > cabal.project
packages: .
constraints: resource-pool < 0.3
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: 3e5ad212dfd777690baa4fef29cd103ddff9ec9b
  subdir: orville-postgresql-libpq
EOF
cat << 'EOF' > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Internal.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Internal.ExecutionResult as O
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlValue as O

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Database.PostgreSQL.LibPQ (Oid(..))
import           Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict', encode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import           Data.String (IsString(fromString))
import qualified Data.Text.Encoding as Enc
import qualified Data.Vector as Vector
EOF
```

Let's suppose we have an example entity with an ID, and some arbitrary JSON
data in a column called 'tags'.

Note how `fooTagsField` below uses the `Value` type from the Aeson library, and the
`jsonb` which we'll define shortly.

Remember that the `Value` contains its own `Null` constructor, which is
distinct from SQL's `NULL`. So we can have JSON nulls in this field, but no SQL
nulls.

We could also use a custom type with `FromJSON`/`ToJSON` instances, since
`jsonb` allows for that too. Aeson is not the focus of this document though.

```shell
cat << 'EOF' >> app/Main.hs
data Foo = Foo
  { fooId :: Int.Int32
  , fooTags :: Value
  }
  deriving Show

fooIdField :: O.FieldDefinition O.NotNull Int.Int32
fooIdField =
  O.integerField "id"

fooTagsField :: O.FieldDefinition O.NotNull Value
fooTagsField =
  O.fieldOfType jsonb "tags"
EOF
```

Before we can define the corresponding `SqlMarshaller`, we'll need to define the
JSONB data type. This is done using the `SqlType` record.

The OID is the Object Identifier, and you can consult
[pg_type.dat](https://github.com/postgres/postgres/blob/728560db7d868b3ded9a8675742083ab89bcff7c/src/include/catalog/pg_type.dat#L444)
in the PostgreSQL repository for the OIDs for built-in types.

See the Haddocks for more details on the record fields of `SqlType`.

```shell
cat << 'EOF' >> app/Main.hs
jsonb :: (ToJSON a, FromJSON a) => O.SqlType a
jsonb =
  O.SqlType
    { O.sqlTypeExpr = RawSql.unsafeFromRawSql (RawSql.fromString "JSONB")
    , O.sqlTypeReferenceExpr = Nothing
    , O.sqlTypeOid = Oid 3802
    , O.sqlTypeMaximumLength = Nothing
    , O.sqlTypeToSql = jsonToSql
    , O.sqlTypeFromSql = jsonFromSql
    , O.sqlTypeDontDropImplicitDefaultDuringMigrate = False
    }
  where
    jsonToSql :: ToJSON a => a -> O.SqlValue
    jsonToSql = O.fromRawBytes . BSL.toStrict . encode
    jsonFromSql :: FromJSON a => O.SqlValue -> Either String a
    jsonFromSql sqlValue = do
      txt <- O.toText sqlValue
      let bs = Enc.encodeUtf8 txt
      eitherDecodeStrict' bs
EOF
```

Let's define the `SqlMarshaller` and the table. This is standard stuff, no
surprises here.

```shell
cat << 'EOF' >> app/Main.hs
fooMarshaller :: O.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> O.marshallField fooId fooIdField
    <*> O.marshallField fooTags fooTagsField

table :: O.TableDefinition (O.HasKey Int.Int32) Foo Foo
table =
  O.mkTableDefinition "json_demo" (O.primaryKey fooIdField) fooMarshaller
EOF
```

With all definitions done, we can write `main`. Orville will also use the parts
of the `SqlType` during migration.

```shell
cat << 'EOF' >> app/Main.hs
main :: IO ()
main = do
  pool <- O.createConnectionPool O.DisableNoticeReporting 1 10 1 (fromString "host=pg user=orville_docs password=orville")
  O.runOrville pool $ do
    AutoMigration.autoMigrateSchema [ AutoMigration.SchemaTable table ]
    _ <- O.deleteEntity table 0
EOF
```

We'll construct a JSON value using the Aeson library, which makes this look
fairly verbose. But imagine that the `Array` value below was read from a file,
or received over HTTP from a web browser.

```shell
cat << 'EOF' >> app/Main.hs
    _ <- O.insertEntity table Foo { fooId = 0
                                  , fooTags = Aeson.Array $ Vector.fromList
                                      [ Aeson.Number 1
                                      , Aeson.Number 2
                                      , Aeson.Number 3
                                      ]
                                  }
    liftIO . print =<< O.findEntity table 0
  Pool.withResource pool $ \connection -> do
EOF
```

Using raw SQL, we can use PostgreSQL's built-in JSONB functions. Let's suppose
we want a row returned for each of the values in the `Array` above.

| ID | Tag |
| -- | --- |
| 0  | 1   |
| 0  | 2   |
| 0  | 3   |

The query below returns such a result.

```shell
cat << 'EOF' >> app/Main.hs
    result <- RawSql.execute connection (RawSql.fromString "SELECT id, jsonb_array_elements(tags) AS tag FROM json_demo")
EOF
```

We can fetch the results with the low-level `readRows` API:

```shell
cat << 'EOF' >> app/Main.hs
    [[_, (_, one)], [_, (_, two)], [_, (_, three)]] <- O.readRows result
    liftIO $ print (O.toText one, O.toText two, O.toText three)
EOF
```

We can also use an `SqlMarshaller`. The types have to match though: the
programmer must ensure correspondence of the `Result` and the `SqlMarshaller`.
If they don't match, `eReadEntities` below will have a `Left` value.

We'll have the `SqlMarshaller` work with tuples and `marshallReadOnlyField`s.
These allow for succintly defining a quick one-off `SqlMarshaller`.

```shell
cat << 'EOF' >> app/Main.hs
    let
      marshaller :: O.SqlMarshaller w (Int.Int32, Value)
      marshaller =
        (,) <$> O.marshallReadOnlyField fooIdField
            <*> O.marshallReadOnlyField (O.fieldOfType jsonb "tag")
    eReadEntities <- SqlMarshaller.marshallResultFromSqlUsingRowIdExtractor
      ErrorDetailLevel.maximalErrorDetailLevel
      (SqlMarshaller.mkRowIdentityExtractor [] result)
      marshaller
      result
    print eReadEntities
EOF
```
# Program output and test

This concludes this tutorial. The expected output is visible just about the EOF:

```shell
cabal build
cat << 'EOF' > json-test.t
$ cd $TESTDIR
$ cp $(cabal list-bin exe:orville-json | tail -n1) $OLDPWD
$ cd $OLDPWD
$ ./orville-json
Just (Foo {fooId = 0, fooTags = Array [Number 1.0,Number 2.0,Number 3.0]})
(Right "1",Right "2",Right "3")
Right [(0,Number 1.0),(0,Number 2.0),(0,Number 3.0)]
EOF
~/.local/bin/prysk json-test.t --indent=0
```
