module AppManagedEntity.CrudTest where

import Control.Monad (void)

import qualified Database.Orville.Oracle as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import AppManagedEntity.Data.Virus
  ( Virus(..)
  , VirusId(..)
  , bpsDiscoveredAt
  , bpsVirusName
  , brnDiscoveredAt
  , brnVirusName
  )
import AppManagedEntity.Schema (schema, virusTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "AppManagedEntity CRUD Test"
      [ testCase "Insert and find" $ do
          let testId = VirusId 1234
              bpsVirus = Virus testId bpsVirusName bpsDiscoveredAt
          run (TestDB.reset schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          assertEqual
            "Virus returned from insertRecord didn't match input virus"
            bpsVirus
            insertedVirus
          foundVirus <- run $ O.findRecord virusTable testId
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just insertedVirus)
            foundVirus
        --
      , testCase "Update" $ do
          let testId = VirusId 1234
              bpsVirus = Virus testId bpsVirusName bpsDiscoveredAt
              brnVirus = Virus testId brnVirusName brnDiscoveredAt
          run (TestDB.reset schema)
          void (run (O.insertRecord virusTable bpsVirus))
          run $ O.updateRecord virusTable testId brnVirus
          --
          -- This value used to be returned by update record. Manually creating
          -- it here for now until that gets sorted out.
          --
          let updatedVirus = brnVirus
          newlyFoundVirus <- run $ O.findRecord virusTable testId
          assertEqual
            "Virus found in database didn't match the values passed in for update"
            (Just updatedVirus)
            newlyFoundVirus
        --
      , testCase "Delete" $ do
          let testId = VirusId 1234
              bpsVirus = Virus testId bpsVirusName bpsDiscoveredAt
          run (TestDB.reset schema)
          foundDeletedVirus <-
            run $ do
              void $ O.insertRecord virusTable bpsVirus
              O.deleteRecord virusTable testId
              O.findRecord virusTable testId
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus
      ]
