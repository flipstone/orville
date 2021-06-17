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

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, NotNull, integerField, unboundedTextField)
import Database.Orville.PostgreSQL.Internal.PrimaryKey (primaryKey)
import Database.Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller, marshallField)
import Database.Orville.PostgreSQL.Internal.TableDefinition (TableDefinition (..))

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
  TableDefinition
    { tableName = Expr.rawTableName "foo"
    , tablePrimaryKey = primaryKey fooIdField
    , tableMarshaller = fooMarshaller
    }

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
