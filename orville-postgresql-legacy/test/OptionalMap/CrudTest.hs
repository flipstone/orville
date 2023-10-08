module OptionalMap.CrudTest where

import qualified Database.Orville.PostgreSQL as O

import Control.Monad (void)
import Data.Monoid ((<>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import OptionalMap.Entity
import qualified TestDB as TestDB

test_optionalMaps :: TestTree
test_optionalMaps =
  testGroup ("crud operations work for RelationalMaps with optional fields")
    [ insertRecordAndSelectFirstTest "Bad map" badCrudEntityTable
    ]

insertRecordAndSelectFirstTest :: String -> O.TableDefinition (CrudEntity CrudEntityId) (CrudEntity ()) CrudEntityId -> TestTree
insertRecordAndSelectFirstTest descr tableDef = TestDB.withOrvilleRun $ \run ->
  testCase ("insertRecord and selectFirst work with " <> descr) $ do
    run (TestDB.reset [O.Table tableDef])
    void $ run (O.insertRecord tableDef testEntity)
    foundComplexField <- fmap crudEntityComplexField <$> run (O.selectFirst tableDef mempty)
    assertEqual
      ("Entity found in database didn't match the inserted values")
      (Just $ crudEntityComplexField testEntity)
      foundComplexField
  where
    testEntity = CrudEntity () (Just $ ComplexField 0 Nothing)
