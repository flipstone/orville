module TransactionTest where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (throwM, try)
import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Database.Orville as O

import AppManagedEntity.Data.Virus (Virus(..), VirusId(..), bpsVirusName)

import AppManagedEntity.Schema (schema, virusTable)

import qualified TestDB as TestDB

bpsVirus :: Virus
bpsVirus = Virus {virusId = VirusId 1, virusName = bpsVirusName}

data FakeError =
  FakeError
  deriving (Eq, Show, Typeable)

instance Exception FakeError

test_transaction :: TestTree
test_transaction =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Transaction Test"
      [ testCase "Commit" $ do
          run (TestDB.reset schema)
          result <-
            run $ do
              void $ O.withTransaction (O.insertRecord virusTable bpsVirus)
              O.findRecord virusTable (virusId bpsVirus)
          assertEqual
            "Unable to find virus that was inserted. Maybe transaction did not commit?"
            (Just bpsVirus)
            result
      , testCase "Rollback" $ do
          run (TestDB.reset schema)
          (transactionResult, findResult) <-
            run $ do
              transactionResult <-
                try $
                O.withTransaction $ do
                  void $ O.insertRecord virusTable bpsVirus
                  void $ throwM FakeError
              findResult <- O.findRecord virusTable (virusId bpsVirus)
              pure (transactionResult, findResult)
          assertEqual
            "Found virus that should not have been committed!"
            Nothing
            findResult
          assertEqual
            "Transaction result was not an error!"
            (Left FakeError)
            transactionResult
      ]
