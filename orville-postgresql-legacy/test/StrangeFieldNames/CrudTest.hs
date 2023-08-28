module StrangeFieldNames.CrudTest where

import qualified Database.Orville.PostgreSQL as O

import Data.Monoid ((<>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

import StrangeFieldNames.Entity
import qualified TestDB as TestDB

test_strangeNames :: TestTree
test_strangeNames =
  testGroup "Column name crud tests"
    [ columnNameCrudTests "snake_case"
    , columnNameCrudTests "camelCase"
    , columnNameCrudTests "order"
    ]

columnNameCrudTests :: String -> TestTree
columnNameCrudTests columnName =
  testGroup ("crud operations work for \"" <> columnName <> "\"")
    [ insertRecordAndSelectFirstTest columnName
    , insertRecordManyAndSelectAllTest columnName
    , updateRecordTest columnName
    , deleteWhereTest columnName
    ]

insertRecordAndSelectFirstTest :: String -> TestTree
insertRecordAndSelectFirstTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase ("insertRecord and selectFirst work with the column \"" <> columnName <> "\"") $ do
    run (TestDB.reset [O.Table tableDef])
    insertedEntity <- run (O.insertRecord tableDef testEntity)
    foundEntity <- run (O.selectFirst tableDef $ O.where_ (columnDef O..== 0))
    assertEqual
      ("Entity found in database didn't match the inserted values using column name \"" <> columnName <> "\"")
      (Just insertedEntity)
      foundEntity
  where
    tableDef = crudEntityTable columnName
    columnDef = crudEntityRenameableField columnName
    testEntity = CrudEntity () 0

insertRecordManyAndSelectAllTest :: String -> TestTree
insertRecordManyAndSelectAllTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase ("insertRecordMany and selectAll work with the column \"" <> columnName <> "\"") $ do
    run (TestDB.reset [O.Table tableDef])
    _ <- run (O.insertRecordMany tableDef testEntities)
    foundEntities <- run $ O.selectAll tableDef $
      O.where_ (columnDef O..> 5)
      <> O.order columnDef O.Descending
    assertEqual ("Found a different number of entities in the database using column name \"" <> columnName <> "\"")
      (length filteredEntities)
      (length foundEntities)
    assertBool ("List was not in the right order using column name \"" <> columnName <> "\"")
      (checkOrder foundEntities)
  where
    tableDef = crudEntityTable columnName
    columnDef = crudEntityRenameableField columnName
    testEntities = CrudEntity () <$> [1..10]
    filteredEntities = filter (\e -> crudEntityField e > 5) testEntities

checkOrder :: [CrudEntity a] -> Bool
checkOrder [] = True
checkOrder (_:[]) = True
checkOrder (x1:x2:xs) = crudEntityField x2 <= crudEntityField x1 && checkOrder (x2:xs)

updateRecordTest :: String -> TestTree
updateRecordTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase ("Updates work with the column \"" <> columnName <> "\"") $ do
    run (TestDB.reset [O.Table tableDef])
    insertedEntity <- run (O.insertRecord tableDef initialEntity)
    let entityId = crudEntityId insertedEntity
    run $ O.updateRecord tableDef entityId newEntity
    updatedEntity <- run $ O.findRecord tableDef entityId
    assertEqual ("The column \"" <> columnName <> "\" was not updated correctly")
      (Just $ crudEntityField newEntity)
      (crudEntityField <$> updatedEntity)
  where
    tableDef = crudEntityTable columnName
    initialEntity = CrudEntity () 1
    newEntity = CrudEntity () 10

deleteWhereTest :: String -> TestTree
deleteWhereTest columnName = TestDB.withOrvilleRun $ \run ->
  testCase ("Deletes work with the column \"" <> columnName <> "\"") $ do
    run (TestDB.reset [O.Table tableDef])
    _ <- run (O.insertRecord tableDef testEntity)
    _ <- run (O.deleteWhere tableDef $ [columnDef O..== 0])
    foundEntity <- run (O.selectFirst tableDef $ O.where_ (columnDef O..== 0))
    assertEqual
      ("Entity found after deletion with column name \"" <> columnName <> "\"")
      Nothing
      foundEntity
  where
    tableDef = crudEntityTable columnName
    columnDef = crudEntityRenameableField columnName
    testEntity = CrudEntity () 0

