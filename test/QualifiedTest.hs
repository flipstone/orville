module QualifiedTest where

import qualified Database.Orville as O

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import AppManagedEntity.Schema.Virus (virusTable, virusNameField)

test_qualified_name :: TestTree
test_qualified_name =
    testGroup
      "QualifiedTest"
      [ testCase "Qualified Names" $
          let whereCondition = O.isNull virusNameField
              qualified = O.whereQualified virusTable whereCondition
              actual = O.whereConditionSql qualified
              expected = "virus.name IS NULL"
          in assertEqual "Expected names to match" expected actual
      ]
