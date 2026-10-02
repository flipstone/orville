{-# LANGUAGE RankNTypes #-}

{- | Property tests comparing compiled plan execution against native plan
  execution over generated data and parameters, including adversarial text,
  shuffled and duplicated parameters, misses, and empty inputs.
-}
module Test.Equivalence
  ( tests
  ) where

import qualified Data.List as List
import Data.String (fromString)
import qualified Hedgehog as HH
import Hedgehog ((===))

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Plan as Plan

import qualified Orville.JsonPlan as JP
import qualified Test.Fixtures as F

tests :: O.ConnectionPool -> HH.Group
tests pool =
  HH.Group
    (fromString "Test.Equivalence")
    [ (fromString "compiled plans match native execution on generated data", plansMatchNative pool)
    , (fromString "empty parameter lists produce empty results", emptyParams pool)
    ]

plansMatchNative :: O.ConnectionPool -> HH.Property
plansMatchNative pool =
  HH.withTests 20 . HH.property $ do
    dataset <- HH.forAll F.genDataset
    hitParams <- HH.forAll (F.genHitParams dataset)
    mixedParams <- HH.forAll (F.genMixedParams dataset)
    results <-
      HH.evalIO . O.runOrville pool $ do
        F.seedDataset dataset
        nativePairs <- Plan.execute (Plan.planList (JP.toPlan F.authorWithBooks)) hitParams
        compiledPairs <- JP.executeJsonPlanList F.authorWithBooks hitParams
        nativeMaybes <- Plan.execute (Plan.planList (JP.toPlan F.maybeAuthor)) mixedParams
        compiledMaybes <- JP.executeJsonPlanList F.maybeAuthor mixedParams
        nativeLibraries <- Plan.execute (Plan.planList (JP.toPlan F.authorLibrary)) hitParams
        compiledLibraries <- JP.executeJsonPlanList F.authorLibrary hitParams
        nativeFlats <- Plan.execute (Plan.planList (JP.toPlan F.flatReport)) hitParams
        compiledFlats <- JP.executeJsonPlanList F.flatReport hitParams
        pure
          ( (nativePairs, compiledPairs)
          , (nativeMaybes, compiledMaybes)
          , (nativeLibraries, compiledLibraries)
          , (nativeFlats, compiledFlats)
          )
    let
      ( (nativePairs, compiledPairs)
        , (nativeMaybes, compiledMaybes)
        , (nativeLibraries, compiledLibraries)
        , (nativeFlats, compiledFlats)
        ) = results

      normalizePair (author, books) =
        (author, List.sortOn F.bookId books)

      normalizeLibrary =
        List.sortOn (F.bookId . fst)

    length compiledPairs === length hitParams
    length compiledMaybes === length mixedParams
    fmap normalizePair nativePairs === fmap normalizePair compiledPairs
    nativeMaybes === compiledMaybes
    fmap normalizeLibrary nativeLibraries === fmap normalizeLibrary compiledLibraries
    nativeFlats === compiledFlats

emptyParams :: O.ConnectionPool -> HH.Property
emptyParams pool =
  HH.withTests 1 . HH.property $ do
    emptyResults <-
      HH.evalIO (O.runOrville pool (JP.executeJsonPlanList F.authorWithBooks []))
    emptyResults === []
