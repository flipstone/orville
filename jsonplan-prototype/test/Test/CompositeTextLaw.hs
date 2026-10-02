{-# LANGUAGE GADTs #-}

{- | Property tests for the composite-text law: for every supported field
  type, parsing the composite text rendering used in compiled boundaries
  must recover libpq's text-mode output byte-identically and decode to the
  same value.
-}
module Test.CompositeTextLaw
  ( tests
  ) where

import Data.String (fromString)
import qualified Data.Time as Time
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as O

import qualified Orville.JsonPlan as JP
import qualified Test.Fixtures as F

tests :: O.ConnectionPool -> HH.Group
tests pool =
  HH.Group
    (fromString "Test.CompositeTextLaw")
    (fmap (probeProperty pool) fieldProbes)

data FieldProbe where
  FieldProbe ::
    (Eq a, Show a) =>
    String ->
    O.FieldDefinition O.NotNull a ->
    HH.Gen a ->
    FieldProbe

probeProperty :: O.ConnectionPool -> FieldProbe -> (HH.PropertyName, HH.Property)
probeProperty pool (FieldProbe probeName fieldDef gen) =
  ( fromString probeName
  , HH.property $ do
      value <- HH.forAll gen
      lawResult <- HH.evalIO (O.runOrville pool (JP.checkCompositeTextLaw fieldDef value))
      lawResult HH.=== Right ()
  )

fieldProbes :: [FieldProbe]
fieldProbes =
  [ FieldProbe
      "integerField"
      (O.integerField "probe")
      (Gen.integral Range.linearBounded)
  , FieldProbe
      "smallIntegerField"
      (O.smallIntegerField "probe")
      (Gen.integral Range.linearBounded)
  , FieldProbe
      "bigIntegerField"
      (O.bigIntegerField "probe")
      (Gen.integral Range.linearBounded)
  , FieldProbe
      "doubleField"
      (O.doubleField "probe")
      (Gen.double (Range.linearFrac (-1e12) 1e12))
  , FieldProbe
      "booleanField"
      (O.booleanField "probe")
      Gen.bool
  , FieldProbe
      "unboundedTextField"
      (O.unboundedTextField "probe")
      (F.genAdversarialText 20)
  , FieldProbe
      "boundedTextField"
      (O.boundedTextField "probe" 10)
      (F.genAdversarialText 10)
  , FieldProbe
      "fixedTextField"
      (O.fixedTextField "probe" 3)
      (F.genAdversarialText 3)
  , FieldProbe
      "dateField"
      (O.dateField "probe")
      genDay
  , FieldProbe
      "utcTimestampField"
      (O.utcTimestampField "probe")
      (Time.UTCTime <$> genDay <*> genDiffTime)
  , FieldProbe
      "localTimestampField"
      (O.localTimestampField "probe")
      (Time.LocalTime <$> genDay <*> genTimeOfDay)
  ]

genDay :: HH.Gen Time.Day
genDay =
  Time.fromGregorian
    <$> Gen.integral (Range.linear 1900 2100)
    <*> Gen.int (Range.linear 1 12)
    <*> Gen.int (Range.linear 1 28)

genDiffTime :: HH.Gen Time.DiffTime
genDiffTime =
  fmap Time.secondsToDiffTime (Gen.integral (Range.linear 0 86399))

genTimeOfDay :: HH.Gen Time.TimeOfDay
genTimeOfDay =
  Time.TimeOfDay
    <$> Gen.int (Range.linear 0 23)
    <*> Gen.int (Range.linear 0 59)
    <*> fmap fromIntegral (Gen.int (Range.linear 0 59))
