{-# LANGUAGE OverloadedStrings #-}

module Test.PgGen
  ( pgText
  , pgDouble
  , pgInt32
  , pgIdentifier
  , pgIdentifierWithPrefix
  , pgUTCTime
  , pgLocalTime
  , pgDay
  , pgJSON
  )
where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

pgText :: HH.Range Int -> HH.Gen T.Text
pgText range =
  Gen.text range $
    Gen.filter (/= '\NUL') Gen.unicode

pgInt32 :: HH.Gen Int32
pgInt32 =
  Gen.integral (Range.linearFrom 0 minBound maxBound)

{- |
  Produces a double value that can be reliably round tripped through
  PostgreSQL, which only allows 15 digits of decimal precision.

  Generating doubles naively using 'Gen.double' produces large numbers with
  more than 15 digits of precisio, so we use 'encodeFloat' to directly ensure
  the precision of large numbers is within PostgreSQL's limit. Precision of
  small numbers is enforced by rounding excess digits off.

@since 1.0.0.0
-}
pgDouble :: HH.Gen Double
pgDouble = do
  let
    -- We use 14 instead of 15 here to allow for the addinitional power
    -- of 10 that may end up coming from the mantissa, based on the
    -- value of 'maxMantissa' below
    maxExpn =
      truncate (logBase 2 (10 ^ (14 :: Int)) :: Double)

    maxMantissa =
      10

  mantissa <- Gen.integral (Range.linearFrom 0 (-maxMantissa) maxMantissa)
  expn <- Gen.integral (Range.linearFrom 0 (-maxExpn) maxExpn)
  pure . roundExcessPrecisionAfterDecimal 15 $ encodeFloat mantissa expn

roundExcessPrecisionAfterDecimal :: Integer -> Double -> Double
roundExcessPrecisionAfterDecimal maxTotalPrecision double =
  let
    digitsBeforeDecimal =
      decimalDigits (truncate double)

    digitsAfterDecimal =
      maxTotalPrecision - digitsBeforeDecimal

    roundingFactor =
      10.0 ^^ digitsAfterDecimal

    roundDouble =
      fromInteger . round
  in
    if digitsAfterDecimal > 0
      then roundDouble (double * roundingFactor) / roundingFactor
      else double

decimalDigits :: Integer -> Integer
decimalDigits n =
  if abs n < 10
    then 1
    else 1 + decimalDigits (quot n 10)

pgIdentifier :: HH.Gen String
pgIdentifier =
  Gen.string (Range.linear 1 63) $ Gen.element pgIdentifierChars

{- |
  Relation names must be unique in PostgreSQL, so we sometimes generate
  names with prefixes to avoid conflicts between different types of
  relations such as tables and indexes. The min length value allows the
  caller to length of the random strings that will be appended to prefix,
  which case be useful to avoid conflicts.

@since 1.0.0.0
-}
pgIdentifierWithPrefix :: String -> Int -> HH.Gen String
pgIdentifierWithPrefix prefix minLength =
  fmap (prefix <>)
    . Gen.string (Range.linear minLength (63 - length prefix))
    . Gen.element
    $ pgIdentifierChars

{- |
  A list of characters to include in identifiers when testing. Not all of these
  are valid in unquoted identifiers -- this helps ensure that Orville is
  properly quoting ids.

@since 1.0.0.0
-}
pgIdentifierChars :: [Char]
pgIdentifierChars =
  ['a' .. 'z']
    <> ['A' .. 'Z']
    <> ['0' .. '9']
    <> "{}[]()<>!?:;_~^'%&"

pgUTCTime :: HH.Gen Time.UTCTime
pgUTCTime =
  Time.UTCTime <$> pgDay <*> pgDiffTime

pgLocalTime :: HH.Gen Time.LocalTime
pgLocalTime =
  Time.LocalTime <$> pgDay <*> pgTimeOfDay

pgDay :: HH.Gen Time.Day
pgDay = do
  year <- Gen.integral (Range.constantFrom 2000 (-4713) 294276)
  month <- Gen.integral (Range.constant 1 12)
  day <- Gen.integral (Range.constant 1 (Time.gregorianMonthLength year month))

  pure (Time.fromGregorian year month day)

pgTimeOfDay :: HH.Gen Time.TimeOfDay
pgTimeOfDay = fmap Time.timeToTimeOfDay pgDiffTime

pgJSON :: HH.Gen T.Text
pgJSON = do
  let
    alphaNumText :: HH.Range Int -> HH.Gen T.Text
    alphaNumText range =
      Gen.text range $ Gen.filter (/= '\NUL') Gen.alphaNum

  jsonKey <- alphaNumText (Range.constant 0 1024)
  jsonValue <- alphaNumText (Range.constant 0 1024)

  pure $ "{\"" <> jsonKey <> "\": \"" <> jsonValue <> "\"}"

pgDiffTime :: HH.Gen Time.DiffTime
pgDiffTime =
  Time.secondsToDiffTime <$> Gen.integral (Range.constant 0 85399)
