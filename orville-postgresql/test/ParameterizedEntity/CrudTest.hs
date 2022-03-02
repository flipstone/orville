{-# LANGUAGE OverloadedStrings #-}
module ParameterizedEntity.CrudTest where

import qualified Database.Orville.PostgreSQL as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import ParameterizedEntity.Data.Virus (bpsVirus, brnVirus, virusId, mutationId, Mutation(..), VirusName(..))
import ParameterizedEntity.Schema (schema, virusTable, mutationTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "ParameterizedEntity CRUD Test"
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
          run $ O.updateRecord virusTable (virusId insertedVirus) brnVirus
          --
          -- This value used to be returned by update record. Manually creating
          -- it here for now until that gets sorted out.
          --
          let updatedVirus = brnVirus {virusId = virusId insertedVirus}
          newlyFoundVirus <-
            run $ O.findRecord virusTable (virusId insertedVirus)
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
              O.deleteRecord virusTable (virusId insertedVirus)
              O.findRecord virusTable (virusId insertedVirus)
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus

      , testCase "Delete - Cascade" $ do
          foundDeletedMutation <- run $ do
            TestDB.reset schema
            insertedVirus <- O.insertRecord virusTable bpsVirus
            insertedMutation <- O.insertRecord mutationTable (Mutation () (VirusName "Bovine unpopular stomachitis") $ virusId insertedVirus)
            O.deleteRecord virusTable (virusId insertedVirus)
            O.findRecord mutationTable (mutationId insertedMutation)
          assertEqual
            "Mutation was found in the database, but it should have been deleted via CASCADE"
            Nothing
            foundDeletedMutation
      ]
