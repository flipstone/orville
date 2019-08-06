module EntityWrapper.CrudTest where

import qualified Database.Orville.Oracle as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import EntityWrapper.Data.Entity (Entity(..))
import EntityWrapper.Data.Virus (bpsVirus, brnVirus)
import EntityWrapper.Schema (schema, virusTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "EntityWrapper CRUD Test"
      [ testCase "Insert and find" $ do
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          foundVirus <- run $ O.findRecord virusTable (entityKey insertedVirus)
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just insertedVirus)
            foundVirus
        --
      , testCase "Update" $ do
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          run $ O.updateRecord virusTable (entityKey insertedVirus) brnVirus
          --
          -- This value used to be returned by update record. Manually creating
          -- it here for now until that gets sorted out.
          --
          let updatedVirus = Entity (entityKey insertedVirus) brnVirus
          newlyFoundVirus <-
            run $ O.findRecord virusTable (entityKey insertedVirus)
          assertEqual
            "Virus found in database didn't match the values passed in for update"
            (Just updatedVirus)
            newlyFoundVirus
        --
      , testCase "Delete" $ do
          run (TestDB.reset schema)
          foundDeletedVirus <-
            run $ do
              insertedVirus <- O.insertRecord virusTable bpsVirus
              O.deleteRecord virusTable (entityKey insertedVirus)
              O.findRecord virusTable (entityKey insertedVirus)
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus
      ]
