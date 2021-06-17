module Test.RecordOperations
  ( recordOperationsTree,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool, withResource)
import qualified Hedgehog as HH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL (insertRecord, runOrville)
import Database.Orville.PostgreSQL.Connection (Connection)

import qualified Test.Entities.Foo as Foo
import qualified Test.TestTable as TestTable

recordOperationsTree :: Pool Connection -> TestTree
recordOperationsTree pool =
  testGroup
    "RecordOperations"
    [ testProperty "insertRecord does not raise an error" . HH.property $ do
        foo <- HH.forAll Foo.generate

        liftIO $ do
          withResource pool $ \connection ->
            TestTable.dropAndRecreateTableDef connection Foo.table
          runOrville pool $ insertRecord Foo.table foo
    ]
