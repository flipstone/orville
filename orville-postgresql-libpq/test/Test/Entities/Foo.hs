module Test.Entities.Foo
  ( Foo (..),
    FooId,
    FooName,
    table,
    generate,
    generateFooWithName,
    generateFooId,
    generateFooName,
    generateList,
    generateListUsing,
    withTable,
    fooIdField,
    fooNameField,
    fooAgeField,
    hasName,
    averageFooAge,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (on)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Pool (Pool, withResource)
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.Connection (Connection)

import qualified Test.PgGen as PgGen
import qualified Test.TestTable as TestTable

type FooId = Int32
type FooName = T.Text
type FooAge = Int32

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  , fooAge :: FooAge
  }
  deriving (Eq, Show)

table :: Orville.TableDefinition (Orville.HasKey FooId) Foo Foo
table =
  Orville.mkTableDefinition "foo" (Orville.primaryKey fooIdField) fooMarshaller

fooMarshaller :: Orville.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> Orville.marshallField fooId fooIdField
    <*> Orville.marshallField fooName fooNameField
    <*> Orville.marshallField fooAge fooAgeField

fooIdField :: Orville.FieldDefinition Orville.NotNull FooId
fooIdField =
  Orville.integerField "id"

fooNameField :: Orville.FieldDefinition Orville.NotNull FooName
fooNameField =
  Orville.unboundedTextField "name"

fooAgeField :: Orville.FieldDefinition Orville.NotNull FooAge
fooAgeField =
  Orville.integerField "age"

generate :: HH.Gen Foo
generate =
  Foo
    <$> generateFooId
    <*> generateFooName
    <*> generateFooAge

generateFooWithName :: FooName -> HH.Gen Foo
generateFooWithName name =
  Foo
    <$> generateFooId
    <*> pure name
    <*> generateFooAge

generateFooId :: HH.Gen FooId
generateFooId =
  PgGen.pgInt32

generateFooName :: HH.Gen FooName
generateFooName =
  PgGen.pgText (Range.constant 0 10)

generateFooAge :: HH.Gen FooAge
generateFooAge =
  Gen.integral (Range.constant minFooAge maxFooAge)

minFooAge :: FooAge
minFooAge = 0

maxFooAge :: FooAge
maxFooAge = 50

averageFooAge :: FooAge
averageFooAge =
  div (minFooAge + maxFooAge) 2

hasName :: FooName -> Foo -> Bool
hasName name foo =
  fooName foo == name

generateList :: HH.Range Int -> HH.Gen [Foo]
generateList =
  flip generateListUsing generate

generateListUsing :: HH.Range Int -> HH.Gen Foo -> HH.Gen [Foo]
generateListUsing range generator =
  fmap
    (List.nubBy ((==) `on` fooId))
    (Gen.list range generator)

withTable :: MonadIO m => Pool Connection -> Orville.Orville a -> m a
withTable pool operation =
  liftIO $ do
    withResource pool $ \connection ->
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
