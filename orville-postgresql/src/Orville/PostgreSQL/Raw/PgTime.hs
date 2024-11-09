{-# LANGUAGE DeriveFunctor #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.PgTime
  ( dayToPostgreSQL
  , day
  , utcTimeToPostgreSQL
  , utcTime
  , localTimeToPostgreSQL
  , localTime
  )
where

import Control.Applicative (optional)
import qualified Data.Attoparsec.ByteString as AttoBS
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.Word as Word

{- | Renders a 'Time.Day' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
dayToPostgreSQL :: Time.Day -> B8.ByteString
dayToPostgreSQL date =
  B8.pack $
    case absDayYear date of
      NegativeYear absDate -> Time.showGregorian absDate <> " BC"
      PositiveYear _ -> Time.showGregorian date

data AbsYearResult a
  = NegativeYear a
  | PositiveYear a
  deriving (Functor)

absDayYear :: Time.Day -> AbsYearResult Time.Day
absDayYear date =
  let
    (y, m, d) = Time.toGregorian date
    absDay = Time.fromGregorian (abs y) m d
  in
    if y < 0
      then NegativeYear absDay
      else PositiveYear absDay

negateDayYear :: Time.Day -> Time.Day
negateDayYear date =
  let
    (y, m, d) = Time.toGregorian date
  in
    Time.fromGregorian (negate y) m d

{- | An Attoparsec parser for parsing 'Time.Day' from YYYY-MM-DD format. Parsing
  fails if given an invalid 'Time.Day'.

@since 1.0.0.0
-}
day :: AttoB8.Parser Time.Day
day = do
  (y, yearCount) <- decimalWithCount <* AttoB8.char '-'
  if yearCount < 4
    then fail "invalid date format"
    else do
      m <- twoDigits <* AttoB8.char '-'
      d <- twoDigits
      mbBc <- optionalBC
      let
        negateFn = maybe id (const negateDayYear) mbBc
      maybe
        (fail "invalid date format")
        (pure . negateFn)
        (Time.fromGregorianValid y m d)

optionalBC :: AttoB8.Parser (Maybe B8.ByteString)
optionalBC = optional . AttoB8.string $ B8.pack " BC"

{- | An Attoparsec parser for parsing 2-digit integral numbers.

@since 1.0.0.0
-}
twoDigits :: Integral a => AttoB8.Parser a
twoDigits = do
  tens <- AttoB8.digit
  ones <- AttoB8.digit
  pure $ fromChar tens * 10 + fromChar ones

fromChar :: Integral a => Char -> a
fromChar c = fromIntegral $ Char.ord c - Char.ord '0'

{- | Renders a 'Time.UTCTime' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
utcTimeToPostgreSQL :: Time.UTCTime -> B8.ByteString
utcTimeToPostgreSQL time =
  let
    format = Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S%Q+00"
  in
    B8.pack $
      case absUtcTimeYear time of
        NegativeYear absTime -> format absTime <> " BC"
        PositiveYear _ -> format time

{- | An Attoparsec parser for parsing 'Time.UTCTime' from an ISO-8601 style
  datetime and timezone with a few PostgreSQL-specific exceptions. See
  'localTime' for more details.

@since 1.0.0.0
-}
utcTime :: AttoB8.Parser Time.UTCTime
utcTime = do
  lt <- localTime
  sign <- AttoB8.satisfy (\char -> char == '+' || char == '-' || char == 'Z')
  if sign == 'Z'
    then pure $ Time.localTimeToUTC Time.utc lt
    else do
      hour <- twoDigits
      minute <- AttoB8.option 0 $ AttoB8.choice [AttoB8.char ':' *> twoDigits, twoDigits]
      second <- AttoB8.option 0 $ AttoB8.char ':' *> twoDigits
      mbBc <- optionalBC
      let
        negateFn = maybe id (const negateUtcTimeYear) mbBc
        offsetSeconds :: Int
        offsetSeconds = (second + minute * 60 + hour * 3600) * if sign == '+' then (-1) else 1
        offsetNominalDiffTime = fromIntegral offsetSeconds
        diffTime = Time.timeOfDayToTime (Time.localTimeOfDay lt)
        utcTimeWithoutOffset = Time.UTCTime (Time.localDay lt) diffTime
      pure . negateFn $ Time.addUTCTime offsetNominalDiffTime utcTimeWithoutOffset

absUtcTimeYear :: Time.UTCTime -> AbsYearResult Time.UTCTime
absUtcTimeYear time =
  fmap
    (`Time.UTCTime` Time.utctDayTime time)
    (absDayYear $ Time.utctDay time)

negateUtcTimeYear :: Time.UTCTime -> Time.UTCTime
negateUtcTimeYear time =
  Time.UTCTime
    (negateDayYear $ Time.utctDay time)
    (Time.utctDayTime time)

{- | Renders a 'Time.LocalTime' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
localTimeToPostgreSQL :: Time.LocalTime -> B8.ByteString
localTimeToPostgreSQL time =
  let
    format = Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S%Q"
  in
    B8.pack $
      case absLocalTimeYear time of
        NegativeYear absTime -> format absTime <> " BC"
        PositiveYear _ -> format time

absLocalTimeYear :: Time.LocalTime -> AbsYearResult Time.LocalTime
absLocalTimeYear time =
  fmap
    (`Time.LocalTime` Time.localTimeOfDay time)
    (absDayYear $ Time.localDay time)

negateLocalTimeYear :: Time.LocalTime -> Time.LocalTime
negateLocalTimeYear time =
  Time.LocalTime
    (negateDayYear $ Time.localDay time)
    (Time.localTimeOfDay time)

{- | An Attoparsec parser for parsing 'Time.LocalTime' from an ISO-8601 style
  datetime with a few exceptions. The separator between the date and time
  is always @\' \'@ and never @\'T\'@.

@since 1.0.0.0
-}
localTime :: AttoB8.Parser Time.LocalTime
localTime = do
  time <- Time.LocalTime <$> day <* AttoB8.char ' ' <*> timeOfDay
  mbBc <- optionalBC
  pure $ maybe id (const negateLocalTimeYear) mbBc time

{- | An Attoparsec parser for parsing 'Time.TimeOfDay' from an ISO-8601 style time.

@since 1.0.0.0
-}
timeOfDay :: AttoB8.Parser Time.TimeOfDay
timeOfDay = do
  h <- twoDigits <* AttoB8.char ':'
  m <- twoDigits
  s <- AttoB8.option 0 (AttoB8.char ':' *> seconds)
  maybe (fail "invalid time format") pure $ Time.makeTimeOfDayValid h m s

{- | An Attoparsec parser for parsing a base-10 number. Returns the number of
  digits consumed. Based off of 'AttoB8.decimal'.

@since 1.0.0.0
-}
decimalWithCount :: Integral a => AttoB8.Parser (a, a)
decimalWithCount = do
  wrds <- AttoBS.takeWhile1 AttoB8.isDigit_w8
  pure (BS.foldl' appendDigit 0 wrds, fromIntegral $ BS.length wrds)

appendDigit :: Integral a => a -> Word.Word8 -> a
appendDigit a w = a * 10 + fromIntegral (w - 48)

{- | An Attoparsec parser for parsing 'Fixed.Pico' from SS[.sss] format. This can
  handle more resolution than PostgreSQL uses, and will truncate the seconds
  fraction if more than 12 digits are present.

@since 1.0.0.0
-}
seconds :: AttoB8.Parser Fixed.Pico
seconds = do
  s <- twoDigits
  (dec, charCount) <- AttoB8.option (0, 0) (AttoB8.char '.' *> decimalWithCount)
  if charCount >= 12
    then pure $ Fixed.MkFixed $ (s * 10 ^ (12 :: Int)) + (dec `div` 10 ^ (charCount - 12))
    else pure $ Fixed.MkFixed $ (s * 10 ^ (12 :: Int)) + (dec * 10 ^ (12 - charCount))
