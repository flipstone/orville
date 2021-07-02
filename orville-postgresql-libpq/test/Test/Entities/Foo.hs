module Test.Entities.Foo
  ( Foo (..),
    table,
    generate,
    generateList,
    withTable,
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

import qualified Database.Orville.PostgreSQL as Orville
import Database.Orville.PostgreSQL.Connection (Connection)

import qualified Test.PGGen as PGGen
import qualified Test.TestTable as TestTable

type FooId = Int32
type FooName = T.Text

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  }
  deriving (Eq, Show)

table :: Orville.TableDefinition FooId Foo Foo
table =
  Orville.mkTableDefiniton "foo" (Orville.primaryKey fooIdField) fooMarshaller

fooMarshaller :: Orville.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> Orville.marshallField fooId fooIdField
    <*> Orville.marshallField fooName fooNameField

fooIdField :: Orville.FieldDefinition Orville.NotNull FooId
fooIdField =
  Orville.integerField "id"

fooNameField :: Orville.FieldDefinition Orville.NotNull FooName
fooNameField =
  Orville.unboundedTextField "name"

generate :: HH.Gen Foo
generate =
  Foo
    <$> PGGen.pgInt32
    <*> PGGen.pgText (Range.constant 0 10)

generateList :: HH.Range Int -> HH.Gen [Foo]
generateList range =
  fmap
    (List.nubBy ((==) `on` fooId))
    (Gen.list range generate)

withTable :: MonadIO m => Pool Connection -> Orville.Orville a -> m a
withTable pool operation =
  liftIO $ do
    withResource pool $ \connection ->
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
