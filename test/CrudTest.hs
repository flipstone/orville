module CrudTest where

import qualified Data.Text as Text

import qualified Database.Orville as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Example.Data.Virus (Virus(..), VirusId(..), VirusName(..))
import Example.Schema (schema, virusTable)
import qualified TestDB as TestDB

test_crud :: TestTree
test_crud =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "CRUD Test"
      [ testCase "Insert and find" $ do
          run (TestDB.reset schema)
          run (O.insertRecord virusTable bpsVirus)
          foundVirus <- run $ O.findRecord virusTable (virusId bpsVirus)
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just bpsVirus)
            foundVirus
        --
      , testCase "Update" $ do
          run (TestDB.reset schema)
          run (O.insertRecord virusTable bpsVirus)
          run $ O.updateRecord virusTable (virusId bpsVirus) brnVirus
          newlyFoundVirus <- run (O.findRecord virusTable (virusId brnVirus))
          assertEqual
            "Virus found in database didn't match the values returned by from update"
            (Just brnVirus)
            newlyFoundVirus
        --
      , testCase "Delete" $ do
          run (TestDB.reset schema)
          foundDeletedVirus <-
            run $ do
              O.insertRecord virusTable bpsVirus
              O.deleteRecord virusTable bpsVirus
              O.findRecord virusTable (virusId bpsVirus)
          assertEqual
            "Virus was found in the database, but it should have been deleted"
            Nothing
            foundDeletedVirus
      ]

bpsVirus :: Virus
bpsVirus =
  Virus
    { virusId = VirusId 1
    , virusName = VirusName (Text.pack "Bovine popular stomachitis")
    }

brnVirus :: Virus
brnVirus =
  Virus
    { virusId = VirusId 2
    , virusName = VirusName (Text.pack "Black raspberry necrosis")
    }
