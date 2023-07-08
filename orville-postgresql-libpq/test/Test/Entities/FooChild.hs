module Test.Entities.FooChild
  ( FooChild (..)
  , isChildOf
  , table
  , fooChildIdField
  , fooChildFooIdField
  , generate
  , generateList
  , withTables
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (on)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Pool (withResource)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen

import qualified Orville.PostgreSQL as Orville

import qualified Test.Entities.Foo as Foo
import qualified Test.PgGen as PgGen
import qualified Test.TestTable as TestTable

type FooChildId = Int32

data FooChild = FooChild
  { fooChildId :: FooChildId
  , fooChildFooId :: Foo.FooId
  }
  deriving (Eq, Show)

isChildOf :: Foo.Foo -> FooChild -> Bool
isChildOf foo child =
  Foo.fooId foo == fooChildFooId child

table :: Orville.TableDefinition (Orville.HasKey FooChildId) FooChild FooChild
table =
  Orville.mkTableDefinition "foo_child" (Orville.primaryKey fooChildIdField) fooChildMarshaller

fooChildMarshaller :: Orville.SqlMarshaller FooChild FooChild
fooChildMarshaller =
  FooChild
    <$> Orville.marshallField fooChildId fooChildIdField
    <*> Orville.marshallField fooChildFooId fooChildFooIdField

fooChildIdField :: Orville.FieldDefinition Orville.NotNull FooChildId
fooChildIdField =
  Orville.integerField "id"

fooChildFooIdField :: Orville.FieldDefinition Orville.NotNull Foo.FooId
fooChildFooIdField =
  Orville.integerField "foo_id"

generate :: [Foo.Foo] -> HH.Gen FooChild
generate foos =
  FooChild
    <$> PgGen.pgInt32
    <*> Gen.element (Foo.fooId <$> foos)

generateList :: HH.Range Int -> [Foo.Foo] -> HH.Gen [FooChild]
generateList range foos =
  fmap
    (List.nubBy ((==) `on` fooChildId))
    (Gen.list range $ generate foos)

withTables :: MonadIO m => Orville.Pool Orville.Connection -> Orville.Orville a -> m a
withTables pool operation =
  liftIO $ do
    withResource pool $ \connection -> do
      TestTable.dropAndRecreateTableDef connection Foo.table
      TestTable.dropAndRecreateTableDef connection table
    Orville.runOrville pool operation
