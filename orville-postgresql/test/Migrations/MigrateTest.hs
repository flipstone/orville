{-# LANGUAGE RankNTypes #-}

module Migrations.MigrateTest where


import qualified Database.Orville.PostgreSQL as O
import Data.Int (Int32, Int64)
import qualified Data.List as List
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import qualified TestDB as TestDB
import Migrations.Entity

test_migrate :: TestTree
test_migrate =
  testGroup "Migration tests"
    [ columnNameMigrationTests "snake_case"
    , columnNameMigrationTests "camelCase"
    -- Test postgreSQL reserved words can be used as column names
    , columnNameMigrationTests "order"
    ]

-- given a column name, test the correct migrations are generated for it.
columnNameMigrationTests :: String -> TestTree
columnNameMigrationTests columnName =
    testGroup (columnName <> " can be used as a column name")
      [ tableCreationTest columnName
      , nullableConstraintTest columnName
      , addAndDropTest columnName
      , alterColumnTypeTest columnName
      ]

tableCreationTest :: String -> TestTree
tableCreationTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase "Creating Tables is idempotent" $ do
    run (TestDB.reset [])
    assertMigrationIdempotent run [O.Table tableDef]
  where
    tableDef :: O.TableDefinition
                  (MigrationEntity Int32 MigrationEntityId)
                  (MigrationEntity Int32 ())
                  MigrationEntityId
    tableDef = migrationEntityTable (O.int32Field columnName)

nullableConstraintTest :: String -> TestTree
nullableConstraintTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase "Adding and Dropping non-null constraints is idempotent" $ do
    run (TestDB.reset [O.Table nonNullableTableDef])
    assertMigrationIdempotent run [O.Table nullableTableDef]
    assertMigrationIdempotent run [O.Table nonNullableTableDef]
  where
    nonNullableTableDef :: O.TableDefinition
                             (MigrationEntity Int32 MigrationEntityId)
                             (MigrationEntity Int32 ())
                             MigrationEntityId
    nonNullableTableDef = migrationEntityTable (O.int32Field columnName)
    nullableTableDef :: O.TableDefinition
                          (MigrationEntity (Maybe Int32) MigrationEntityId)
                          (MigrationEntity (Maybe Int32) ())
                          MigrationEntityId
    nullableTableDef = migrationEntityTable (O.nullableField $ O.int32Field columnName)

addAndDropTest :: String -> TestTree
addAndDropTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase "Adding and Dropping columns is idempotent" $ do
    run (TestDB.reset [O.Table tableDef])
    assertMigrationIdempotent run [O.Table $ migrationEntityTableWithDroppedColumn columnName]
    assertMigrationIdempotent run [O.Table tableDef]
  where
    tableDef :: O.TableDefinition
                  (MigrationEntity Int32 MigrationEntityId)
                  (MigrationEntity Int32 ())
                  MigrationEntityId
    tableDef = migrationEntityTable (O.int32Field columnName)

alterColumnTypeTest :: String -> TestTree
alterColumnTypeTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase "Modifying column type is idempotent" $ do
    run (TestDB.reset [O.Table int32TableDef])
    assertMigrationIdempotent run [O.Table int64TableDef]
  where
    int32TableDef :: O.TableDefinition
                  (MigrationEntity Int32 MigrationEntityId)
                  (MigrationEntity Int32 ())
                  MigrationEntityId
    int32TableDef = migrationEntityTable (O.int32Field columnName)
    int64TableDef :: O.TableDefinition
                  (MigrationEntity Int64 MigrationEntityId)
                  (MigrationEntity Int64 ())
                  MigrationEntityId
    int64TableDef = migrationEntityTable (O.int64Field columnName)

assertMigrationIdempotent :: HasCallStack => TestDB.OrvilleRunner -> O.SchemaDefinition -> IO ()
assertMigrationIdempotent run migrationSchema = do
  trace <- run (TestDB.queryTrace isDDL (O.migrateSchema migrationSchema))
  assertBool
    "Expected migration trace not to be empty, but it was!"
    (not (null trace))
  plan <- run (O.generateMigrationPlan migrationSchema)
  case plan of
    Nothing -> pure ()
    Just somethingToDo ->
      assertFailure $
      "Expected migration plan to be Nothing when database is already migrated, but ddl was:\n" ++
      showDDL somethingToDo

isDDL :: O.QueryType -> Bool
isDDL qt = qt == O.DDLQuery

showDDL :: O.MigrationPlan -> String
showDDL =
  List.intercalate "\n\n" . map O.migrationItemDDL . O.migrationPlanItems

