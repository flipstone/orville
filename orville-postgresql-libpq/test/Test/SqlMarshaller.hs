module Test.SqlMarshaller
  ( sqlMarshallerTree
  ) where

import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty (TestTree, testGroup)

import Database.Orville.PostgreSQL.Internal.SqlMarshaller (marshallFromSql)

sqlMarshallerTree :: TestTree
sqlMarshallerTree =
  testGroup "SqlMarshaller properties" [ roundTripSqlMarshaller
                                       , canCombineSqlMarshaller
                                       ]

roundTripSqlMarshaller :: TestTree
roundTripSqlMarshaller =
  testProperty "Can round trip an int through SqlMarshaller" . HH.property $ do
    someInt <- HH.forAll generateInt
    (marshallFromSql (pure someInt) []) HH.=== (Right someInt)

canCombineSqlMarshaller :: TestTree
canCombineSqlMarshaller =
  testProperty "Can combine SqlMarshallers with <*>" . HH.property $ do
    firstInt <- HH.forAll generateInt
    secondInt <- HH.forAll generateInt
    marshallFromSql ((pure (+ firstInt)) <*> (pure secondInt)) [] HH.=== (Right (firstInt + secondInt))

generateInt :: HH.MonadGen m => m Int
generateInt = Gen.int $ Range.exponential 1 1024
