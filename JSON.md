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
sed -i -re 's/build-depends:/build-depends: orville-postgresql, aeson, bytestring, postgresql-libpq, resource-pool, text, vector,/' *.cabal
cat << 'EOF' > cabal.project
packages: .
source-repository-package
  type: git
  location: https://github.com/flipstone/orville.git
  tag: 82fc9d4d93a24440fe3c9d34a75a4a83acde131b
  subdir: orville-postgresql
EOF
cat << 'EOF' > app/Main.hs
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.AutoMigration as AutoMigration
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson (Value, eitherDecodeStrict')
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import           Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.Lazy as LazyText
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
  aesonValueField "tags"
EOF
```

Before we can define the corresponding `SqlMarshaller`, we'll need to define the
`aesonValueField` helper function. This is done `tryConvertSqlType` along with
`jsonb` field to apply Aeson encoding and decode.

```shell
cat << 'EOF' >> app/Main.hs
aesonValueField :: String -> O.FieldDefinition O.NotNull Value
aesonValueField name =
  O.convertField
    (O.tryConvertSqlType encodeJSON decodeJSON)
    (O.jsonbField name)

decodeJSON :: T.Text -> Either String Value
decodeJSON =
  eitherDecodeStrict' . Enc.encodeUtf8

encodeJSON :: Value -> T.Text
encodeJSON =
  LazyText.toStrict . encodeToLazyText
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
EOF
```

Using raw SQL, we can use PostgreSQL's built-in JSONB functions. Let's suppose
we want a row returned for each of the values in the `Array` above.

| ID | Tag |
| -- | --- |
| 0  | 1   |
| 0  | 2   |
| 0  | 3   |

We can use an `SqlMarshaller` to produce a result like this, even though there
is no table for the returned schema. The programmer must ensure correspondence
of the SQL and the `SqlMarshaller`. If they don't match, an exception will be
thrown.

We'll have the `SqlMarshaller` work with tuples and `marshallReadOnlyField`s.
These allow for succintly defining a quick one-off `SqlMarshaller`.

```shell
cat << 'EOF' >> app/Main.hs
    let
      marshaller :: O.SqlMarshaller w (Int.Int32, Value)
      marshaller =
        (,) <$> O.marshallReadOnlyField fooIdField
            <*> O.marshallReadOnlyField (aesonValueField "tag")
    readEntities <-
      O.executeAndDecode
        O.SelectQuery
        (RawSql.fromString "SELECT id, jsonb_array_elements(tags) AS tag FROM json_demo")
        (Marshall.annotateSqlMarshallerEmptyAnnotation marshaller)
    liftIO $ print readEntities
EOF
```
# Program output and test

This concludes this tutorial. The expected output is visible just above the EOF:

```shell
cabal build
cat << 'EOF' > json-test.t
$ cd $TESTDIR
$ cp $(cabal list-bin exe:orville-json | tail -n1) $OLDPWD
$ cd $OLDPWD
$ ./orville-json
Just (Foo {fooId = 0, fooTags = Array [Number 1.0,Number 2.0,Number 3.0]})
[(0,Number 1.0),(0,Number 2.0),(0,Number 3.0)]
EOF
~/.local/bin/prysk json-test.t --indent=0
```
