module AppManagedEntity.CrudTest where

import Control.Monad (void)
import qualified Data.Map as Map

import qualified Database.Orville.PostgreSQL as O

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

      , testCase "Insert and find many" $ do
          let bpsId = VirusId 1234
              brnId = VirusId 5678

              bpsVirus = Virus bpsId bpsVirusName bpsDiscoveredAt
              brnVirus = Virus brnId brnVirusName brnDiscoveredAt

          run (TestDB.reset schema)
          () <- run (O.insertRecordMany virusTable [bpsVirus, brnVirus])

          foundViruses <- run $ O.findRecords virusTable [bpsId, brnId]
          assertEqual
            "Viruses found in database didn't match the originally inserted values"
            (Map.fromList [(bpsId, bpsVirus), (brnId, brnVirus)])
            foundViruses

      , testCase "Insert many returning" $ do
          let bpsId = VirusId 1234
              brnId = VirusId 5678

              bpsVirus = Virus bpsId bpsVirusName bpsDiscoveredAt
              brnVirus = Virus brnId brnVirusName brnDiscoveredAt
              viruses = [bpsVirus, brnVirus]

          run (TestDB.reset schema)
          insertedViruses <- run (O.insertRecordManyReturning virusTable viruses)

          assertEqual
            "Viruses returned from insertRecordManyReturning didn't match input viruses"
            viruses
            insertedViruses

          foundViruses <- run $ O.findRecords virusTable $ virusId <$> viruses
          assertEqual
            "Viruses found in database didn't match the originally inserted values"
            (Map.fromList $ zip (virusId <$> viruses) viruses)
            foundViruses

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
