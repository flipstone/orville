{-# LANGUAGE RankNTypes #-}

module CrudTest where

import Control.Monad (void)
import Data.Convertible (convert)
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Data.Text as Text
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.PostgreSQL as Postgres
import System.Environment (getEnv)

import qualified Database.Orville as O
import qualified Database.Orville.Raw as ORaw

import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertEqual, testCase)

import Example.Data.Virus (Virus(..), VirusId(..), VirusName(..))
import Example.Schema (schema, virusTable)

type TestPool = Pool Postgres.Connection

test_crud :: TestTree
test_crud =
  withOrvilleRun $ \run ->
    testGroup
      "CRUD Test"
      [ testCase "Insert and find" $ do
          run (resetToBlankSchema schema)
          insertedVirus <- run (O.insertRecord virusTable bpsVirus)
          foundVirus <- run $ O.findRecord virusTable (virusId insertedVirus)
          assertEqual
            "Virus found in database didn't match the originally inserted values"
            (Just insertedVirus)
            foundVirus
        --
      , testCase "Update" $ do
          run (resetToBlankSchema schema)
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
          run (resetToBlankSchema schema)
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

resetToBlankSchema :: O.SchemaDefinition -> O.Orville ()
resetToBlankSchema schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
    -- I would like to use placeholders here, but postgres gives my a
    -- sql syntax error when I do :(
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef

withOrvilleRun :: ((forall a. O.Orville a -> IO a) -> TestTree) -> TestTree
withOrvilleRun mkOrvilleTree = withDb (\pool -> mkOrvilleTree (run pool))
  where
    run :: IO TestPool -> forall a. O.Orville a -> IO a
    run getPool action = do
      pool <- getPool
      O.runOrville action (O.newOrvilleEnv pool)

withDb :: (IO TestPool -> TestTree) -> TestTree
withDb = withResource acquirePool destroyAllResources

acquirePool :: IO TestPool
acquirePool = do
  connString <- getEnv "TEST_CONN_STRING"
  createPool (Postgres.connectPostgreSQL' connString) HDBC.disconnect 1 60 1
