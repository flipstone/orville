module CompositePrimaryKey.CrudTest where

import Control.Monad (void)
import qualified Control.Monad.Catch as Catch
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Database.HDBC as HDBC

import qualified Database.Orville.PostgreSQL as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

import CompositePrimaryKey.Data.Virus
  ( Virus(..)
  , VirusName(..)
  , bpsVirus
  , brnVirus
  , virusKey
  )
import CompositePrimaryKey.Schema (schema, virusTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "CompositePrimaryKey CRUD Test"
      [ testCase "Insert and find" $ do
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          assertEqual
            "Virus returned from insertRecord didn't match input virus"
            bpsVirus
            insertedVirus
          foundVirus <- run $ O.findRecord virusTable (virusKey bpsVirus)
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just insertedVirus)
            foundVirus

      , testCase "Insert and find many" $ do
          let
            bpsKey = virusKey bpsVirus
            brnKey = virusKey brnVirus

          run (TestDB.reset schema)
          () <- run (O.insertRecordMany virusTable [bpsVirus, brnVirus])

          foundViruses <- run $ O.findRecords virusTable [bpsKey, brnKey]
          assertEqual
            "Viruses found in database didn't match the originally inserted values"
            (Map.fromList [(bpsKey, bpsVirus), (brnKey, brnVirus)])
            foundViruses

      , testCase "Update" $ do
          let
            newBPSVirus =
              bpsVirus
                { virusName = VirusName (T.pack "Bovine UNpopular stomachitis")
                }

          run (TestDB.reset schema)
          void (run (O.insertRecord virusTable bpsVirus))
          run $ O.updateRecord virusTable (virusKey bpsVirus) newBPSVirus

          newlyFoundVirus <- run $ O.findRecord virusTable (virusKey bpsVirus)
          assertEqual
            "Virus found in database didn't match the values passed in for update"
            (Just newBPSVirus)
            newlyFoundVirus
        --
      , testCase "Delete" $ do
          run (TestDB.reset schema)
          foundDeletedVirus <-
            run $ do
              void $ O.insertRecord virusTable bpsVirus
              O.deleteRecord virusTable (virusKey bpsVirus)
              O.findRecord virusTable (virusKey bpsVirus)
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus

      , testCase "Insert conflicting primary keys" $ do
          run (TestDB.reset schema)
          eitherErrorVirus <- run $ do
            void $ O.insertRecord virusTable bpsVirus
            Catch.try (O.insertRecord virusTable bpsVirus)

          case eitherErrorVirus of
            Left sqlError ->
              assertEqual
                "Expected a duplicate key constraint violation"
                "23505"
                (HDBC.seState sqlError)

            Right _ ->
              assertFailure "Expected second insert to fail, but it did not!"
      ]
