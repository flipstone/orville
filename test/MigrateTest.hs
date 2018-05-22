module MigrateTest where

import qualified Database.Orville as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import ParameterizedEntity.Schema (schema)
import qualified TestDB as TestDB

test_migrate :: TestTree
test_migrate =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Migrate Test"
      [ testCase "Migration is idempotent" $ do
          run (TestDB.reset [])
          firstTrace <- run (TestDB.queryTrace isDDL (O.migrateSchema schema))
          assertBool
            "Expected first migration trace not to be empty, but it was!"
            (not (null firstTrace))
          secondTrace <- run (TestDB.queryTrace isDDL (O.migrateSchema schema))
          assertEqual
            "Expected second migration trace to be empty"
            []
            secondTrace
      ]

isDDL :: O.QueryType -> Bool
isDDL qt = qt == O.DDLQuery
