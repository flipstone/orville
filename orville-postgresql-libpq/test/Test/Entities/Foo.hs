module Test.Entities.Foo
  ( Foo (..),
    table,
    generate,
  )
where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range

import Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, NotNull, integerField, unboundedTextField)
import Orville.PostgreSQL.Internal.PrimaryKey (primaryKey)
import Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller, marshallField)
import Orville.PostgreSQL.Internal.TableDefinition (TableDefinition, mkTableDefiniton)

import qualified Test.PGGen as PGGen

type FooId = Int32
type FooName = T.Text

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  }
  deriving (Eq, Show)

table :: TableDefinition FooId Foo Foo
table =
  mkTableDefiniton "foo" (primaryKey fooIdField) fooMarshaller

fooMarshaller :: SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> marshallField fooId fooIdField
    <*> marshallField fooName fooNameField

fooIdField :: FieldDefinition NotNull FooId
fooIdField =
  integerField "id"

fooNameField :: FieldDefinition NotNull FooName
fooNameField =
  unboundedTextField "name"

generate :: HH.Gen Foo
generate =
  Foo
    <$> PGGen.pgInt32
    <*> PGGen.pgText (Range.constant 0 10)
