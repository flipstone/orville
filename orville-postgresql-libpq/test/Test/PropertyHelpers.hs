module Test.PropertyHelpers
  ( testPropertyOnce,
  )
where

import qualified Hedgehog as HH
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

testPropertyOnce :: TestName -> HH.Property -> TestTree
testPropertyOnce description =
  testProperty description . HH.withTests 1
