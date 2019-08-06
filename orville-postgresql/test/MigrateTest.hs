module MigrateTest where

import qualified Data.List as List

import qualified Database.Orville.PostgreSQL as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

import ParameterizedEntity.Schema (schema)
import qualified TestDB as TestDB

test_migrate :: TestTree
test_migrate =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Migrate Test"
      [ testCase "Migration is idempotent" $ do
          run (TestDB.reset [])
          trace <- run (TestDB.queryTrace isDDL (O.migrateSchema schema))
          assertBool
            "Expected first migration trace not to be empty, but it was!"
            (not (null trace))
          plan <- run (O.generateMigrationPlan schema)
          case plan of
            Nothing -> pure ()
            Just somethingToDo ->
              assertFailure $
              "Expected migration plan to be Nothing when database is already migrated, but ddl was:\n" ++
              showDDL somethingToDo
      ]

isDDL :: O.QueryType -> Bool
isDDL qt = qt == O.DDLQuery

showDDL :: O.MigrationPlan -> String
showDDL =
  List.intercalate "\n\n" . map O.migrationItemDDL . O.migrationPlanItems
