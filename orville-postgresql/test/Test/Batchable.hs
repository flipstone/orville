module Test.Batchable
  ( batchableTests
  ) where

import qualified Data.List.NonEmpty as NE
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Batchable as Batchable

batchableTests :: Tasty.TestTree
batchableTests =
  Tasty.testGroup
    "Batchable"
    [ TastyHH.testProperty
        "(toUnbatched . batchNonEmpty) returns the original list"
        prop_nonEmptyToUnbatched
    , TastyHH.testProperty
        "(toBatched . batchNonEmpty) returns all items in the original list, in order"
        prop_nonEmptyToBatchedReturnsAllItems
    , TastyHH.testProperty
        "(toBatched . batchNonEmpty) sizes batches correctly"
        prop_nonEmptyToBatchedSizesBatchesCorrectly
    ]

prop_nonEmptyToUnbatched :: HH.Property
prop_nonEmptyToUnbatched =
  HH.property $ do
    autoSize <- HH.forAll (Gen.int (Range.linear 1 10))
    list <- HH.forAll $ Gen.nonEmpty (Range.linear 1 100) (Gen.int (Range.linear 10 99))
    (Batchable.toUnbatched . Batchable.batchNonEmpty autoSize $ list) === list

prop_nonEmptyToBatchedReturnsAllItems :: HH.Property
prop_nonEmptyToBatchedReturnsAllItems =
  HH.property $ do
    autoSize <- HH.forAll (Gen.int (Range.linear 1 10))
    list <- HH.forAll $ Gen.nonEmpty (Range.linear 1 100) (Gen.int (Range.linear 10 99))
    batchSize <-
      HH.forAll $
        Gen.frequency
          [ (1, pure Batchable.BatchSizeAuto)
          , (5, Batchable.BatchSize <$> Gen.int (Range.linear 1 10))
          ]

    let
      allBatchedItems =
        concat
          . map NE.toList
          . Batchable.toBatched batchSize
          . Batchable.batchNonEmpty autoSize
          $ list

    allBatchedItems === NE.toList list

prop_nonEmptyToBatchedSizesBatchesCorrectly :: HH.Property
prop_nonEmptyToBatchedSizesBatchesCorrectly =
  HH.property $ do
    autoSize <- HH.forAll (Gen.int (Range.linear 1 10))
    list <- HH.forAll $ Gen.nonEmpty (Range.linear 1 100) (Gen.int (Range.linear 10 99))
    batchSize <-
      HH.forAll $
        Gen.frequency
          [ (1, pure Batchable.BatchSizeAuto)
          , (5, Batchable.BatchSize <$> Gen.int (Range.linear 1 10))
          ]

    let
      itemCount =
        NE.length list

      expectedBatchSize =
        case batchSize of
          Batchable.BatchSizeAuto -> autoSize
          Batchable.BatchSize size -> size

      expectedBatchCount =
        itemCount `div` expectedBatchSize

      leftoverCount =
        itemCount - (expectedBatchCount * expectedBatchSize)

      expectedLeftovers =
        if leftoverCount > 0
          then [leftoverCount]
          else []

      expectedBatchSizes =
        replicate expectedBatchCount expectedBatchSize
          <> expectedLeftovers

      actualBatchSizes =
        map NE.length
          . Batchable.toBatched batchSize
          . Batchable.batchNonEmpty autoSize
          $ list

    actualBatchSizes === expectedBatchSizes
