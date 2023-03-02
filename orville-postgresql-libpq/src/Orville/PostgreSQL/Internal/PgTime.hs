module Orville.PostgreSQL.Internal.PgTime
  ( dayToPostgreSQL,
    day,
    utcTimeToPostgreSQL,
    utcTime,
    localTimeToPostgreSQL,
    localTime,
  )
where

import qualified Data.Attoparsec.ByteString as AttoBS
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.Word as Word

{- |
  Renders a 'Time.Day' value to a textual representation for PostgreSQL
-}
dayToPostgreSQL :: Time.Day -> B8.ByteString
dayToPostgreSQL =
  B8.pack . Time.showGregorian

{- |
  An Attoparsec parser for parsing 'Time.Day' from YYYY-MM-DD format. Parsing
  fails if given an invalid day.
-}
day :: AttoB8.Parser Time.Day
day = do
  (y, yearCount) <- decimalWithCount <* AttoB8.char '-'
  if yearCount < 4
    then fail "invalid date format"
    else do
      m <- twoDigits <* AttoB8.char '-'
      d <- twoDigits
      maybe (fail "invalid date format") pure $ Time.fromGregorianValid y m d

{- |
  An Attoparsec parser for parsing 2 digit integral numbers.
-}
twoDigits :: Integral a => AttoB8.Parser a
twoDigits = do
  tens <- AttoB8.digit
  ones <- AttoB8.digit
  pure $ fromChar tens * 10 + fromChar ones

fromChar :: Integral a => Char -> a
fromChar c = fromIntegral $ Char.ord c - Char.ord '0'

{- |
  Renders a 'Time.UTCTime' value to a textual representation for PostgreSQL
-}
utcTimeToPostgreSQL :: Time.UTCTime -> B8.ByteString
utcTimeToPostgreSQL =
  B8.pack . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S%Q+00"

{- |
  An Attoparsec parser for parsing 'Time.UTCTime' from an ISO 8601 style
  datetime and timezone with a few postgresql specific exceptions. See
  localTime for more details
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
      let offsetSeconds :: Int
          offsetSeconds = (second + minute * 60 + hour * 3600) * if sign == '+' then (-1) else 1
          offsetNominalDiffTime = fromIntegral offsetSeconds
          diffTime = Time.timeOfDayToTime (Time.localTimeOfDay lt)
          utcTimeWithoutOffset = Time.UTCTime (Time.localDay lt) diffTime
      pure $ Time.addUTCTime offsetNominalDiffTime utcTimeWithoutOffset

{- |
  Renders a 'Time.LocalTime value to a textual representation for PostgreSQL
-}
localTimeToPostgreSQL :: Time.LocalTime -> B8.ByteString
localTimeToPostgreSQL =
  B8.pack . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S%Q"

{- |
  An Attoparsec parser for parsing 'Time.LocalTime' from an ISO 8601 style
  datetime with a few exceptions. The seperator between the date and time
  is always ' ' and never 'T'.
-}
localTime :: AttoB8.Parser Time.LocalTime
localTime = do
  Time.LocalTime <$> day <* AttoB8.char ' ' <*> timeOfDay

{- |
  An Attoparsec parser for parsing 'Time.TimeOfDay' from an ISO 8601 style time.
-}
timeOfDay :: AttoB8.Parser Time.TimeOfDay
timeOfDay = do
  h <- twoDigits <* AttoB8.char ':'
  m <- twoDigits
  s <- AttoB8.option 0 (AttoB8.char ':' *> seconds)
  maybe (fail "invalid time format") pure $ Time.makeTimeOfDayValid h m s

{- |
  An Attoparsec parser for parsing a base 10 number and returns the number of
  digits consumed. Based off of AttoB8.decimal.
-}
decimalWithCount :: Integral a => AttoB8.Parser (a, a)
decimalWithCount = do
  wrds <- AttoBS.takeWhile1 AttoB8.isDigit_w8
  pure (BS.foldl' appendDigit 0 wrds, fromIntegral $ BS.length wrds)

appendDigit :: Integral a => a -> Word.Word8 -> a
appendDigit a w = a * 10 + fromIntegral (w - 48)

{- |
  An Attoparsec parser for parsing 'Fixed.Pico' from SS[.sss] format. This can
  handle more resolution than postgres uses, and will truncate the seconds
  fraction if more than 12 digits are present.
-}
seconds :: AttoB8.Parser Fixed.Pico
seconds = do
  s <- twoDigits
  (dec, charCount) <- AttoB8.option (0, 0) (AttoB8.char '.' *> decimalWithCount)
  if charCount >= 12
    then pure $ Fixed.MkFixed $ (s * 10 ^ (12 :: Int)) + (dec `div` 10 ^ (charCount - 12))
    else pure $ Fixed.MkFixed $ (s * 10 ^ (12 :: Int)) + (dec * 10 ^ (12 - charCount))
