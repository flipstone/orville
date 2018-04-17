module CrudTest where

import qualified Database.Orville as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Example.Data.Virus (bpsVirus, brnVirus, virusId, virusName)
import Example.Schema (schema, virusTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "CRUD Test"
      [ testCase "Insert and find" $ do
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          foundVirus <- run $ O.findRecord virusTable (virusId insertedVirus)
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just insertedVirus)
            foundVirus
        --
      , testCase "Update" $ do
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          updatedVirus <-
            run $ O.updateRecord virusTable (virusId insertedVirus) brnVirus
          newlyFoundVirus <-
            run $ O.findRecord virusTable (virusId insertedVirus)
          assertEqual
            "Virus returned from update didn't match the values passed in for update"
            (virusName brnVirus)
            (virusName updatedVirus)
          assertEqual
            "Virus found in database didn't match the values returned by from update"
            (Just updatedVirus)
            newlyFoundVirus
        --
      , testCase "Delete" $ do
          run (TestDB.reset schema)
          foundDeletedVirus <-
            run $ do
              insertedVirus <- O.insertRecord virusTable bpsVirus
              O.deleteRecord virusTable insertedVirus
              O.findRecord virusTable (virusId insertedVirus)
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus
      ]
