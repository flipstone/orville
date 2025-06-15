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

import Control.Applicative ((<|>))
import Control.Monad (when)
import qualified Data.Attoparsec.ByteString as AttoBS
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Data.Word as Word
import qualified Text.Printf as Printf

{- | Renders a 'Time.Day' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
dayToPostgreSQL :: Time.Day -> B8.ByteString
dayToPostgreSQL date =
  case mkCommonEraDay date of
    (ce, ced) ->
      B8.pack (renderCommonEraDay ced <> renderCommonEraSuffix ce)

{- | An Attoparsec parser for parsing 'Time.Day' from a string in PostgreSQL's ISO style
  (YYYY-MM-DD[ BC]) format. Parsing fails if given an invalid 'Time.Day'.

@since 1.0.0.0
-}
day :: AttoB8.Parser Time.Day
day = do
  ced <- commonEraDay
  ce <- parseCommonEraSuffix
  commonEraDayToISO8601Day ce ced

commonEraDay :: AttoB8.Parser CommonEraDay
commonEraDay = do
  (y, yearCount) <- decimalWithCount <* AttoB8.char '-'
  when (yearCount < 4) (fail "invalid date format")
  m <- twoDigits <* AttoB8.char '-'
  d <- twoDigits
  pure $ CommonEraDay y m d

{- | Renders a 'Time.UTCTime' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
utcTimeToPostgreSQL :: Time.UTCTime -> B8.ByteString
utcTimeToPostgreSQL time =
  let
    -- DiffTime lacks support for %Q (fractional seconds), so be sure to format
    -- via a type that supports %Q.
    formattedTime =
      Time.formatTime Time.defaultTimeLocale "%0H:%0M:%0S%Q+00"
        . Time.timeToTimeOfDay
        $ Time.utctDayTime time
  in
    case mkCommonEraDay (Time.utctDay time) of
      (ce, ced) ->
        B8.pack $
          renderCommonEraDay ced <> " " <> formattedTime <> renderCommonEraSuffix ce

{- | An Attoparsec parser for 'Time.UTCTime' from a PostgreSQL ISO style timestamptz format.

@since 1.0.0.0
-}
utcTime :: AttoB8.Parser Time.UTCTime
utcTime = do
  ced <- commonEraDay <* AttoB8.char ' '
  time <- timeOfDay
  offset <- signedTimezoneOffset
  ce <- parseCommonEraSuffix
  validDay <- commonEraDayToISO8601Day ce ced
  pure $ Time.addUTCTime offset (Time.UTCTime validDay (Time.timeOfDayToTime time))

signedTimezoneOffset :: AttoB8.Parser Time.NominalDiffTime
signedTimezoneOffset = do
  sign <- AttoB8.satisfy (\char -> char == '+' || char == '-')
  case sign of
    '+' -> fmap negate timezoneOffset
    _ -> timezoneOffset

timezoneOffset :: AttoB8.Parser Time.NominalDiffTime
timezoneOffset = do
  h <- twoDigits
  m <- AttoB8.option 0 (AttoB8.char ':' *> twoDigits)
  s <- AttoB8.option 0 (AttoB8.char ':' *> twoDigits)
  pure . fromIntegral $ (s :: Int) + m * 60 + h * 3600

{- | Renders a 'Time.LocalTime' value to a textual representation for PostgreSQL.

@since 1.0.0.0
-}
localTimeToPostgreSQL :: Time.LocalTime -> B8.ByteString
localTimeToPostgreSQL time =
  let
    formattedTime =
      Time.formatTime Time.defaultTimeLocale "%0H:%0M:%0S%Q" $
        Time.localTimeOfDay time
  in
    case mkCommonEraDay (Time.localDay time) of
      (ce, ced) ->
        B8.pack $
          renderCommonEraDay ced <> " " <> formattedTime <> renderCommonEraSuffix ce

{- | An Attoparsec parser for 'Time.LocalTime' from PostgreSQL's ISO style timestamp format.

@since 1.0.0.0
-}
localTime :: AttoB8.Parser Time.LocalTime
localTime = do
  ced <- commonEraDay <* AttoB8.char ' '
  time <- timeOfDay
  ce <- parseCommonEraSuffix
  validDay <- commonEraDayToISO8601Day ce ced
  pure $ Time.LocalTime validDay time

{- | An Attoparsec parser for 'Time.TimeOfDay' from PostgreSQL's ISO style time format.

@since 1.0.0.0
-}
timeOfDay :: AttoB8.Parser Time.TimeOfDay
timeOfDay = do
  h <- twoDigits <* AttoB8.char ':'
  m <- twoDigits <* AttoB8.char ':'
  s <- seconds
  case Time.makeTimeOfDayValid h m s of
    Nothing -> fail "invalid time format"
    Just validTime -> pure validTime

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

{- | An Attoparsec parser for parsing 2-digit integral numbers.

@since 1.0.0.0
-}
twoDigits :: Integral a => AttoB8.Parser a
twoDigits = do
  tens <- fmap (fromIntegral . Char.digitToInt) AttoB8.digit
  ones <- fmap (fromIntegral . Char.digitToInt) AttoB8.digit
  pure $ tens * 10 + ones

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

data CommonEra
  = BCE
  | CE

renderCommonEraSuffix :: CommonEra -> String
renderCommonEraSuffix ce =
  case ce of
    BCE -> " BC"
    CE -> mempty

parseCommonEraSuffix :: AttoB8.Parser CommonEra
parseCommonEraSuffix =
  (BCE <$ AttoB8.string (B8.pack " BC") <|> pure CE) <* AttoB8.endOfInput

data CommonEraDay = CommonEraDay
  { _commonEraDayYear :: Integer
  , _commonEraDayMonth :: Int
  , _commonEraDayDay :: Int
  }

mkCommonEraDay :: Time.Day -> (CommonEra, CommonEraDay)
mkCommonEraDay date =
  case Time.toGregorian date of
    (y, m, d) ->
      fmap
        (\cey -> CommonEraDay cey m d)
        (if y <= 0 then (BCE, 1 - y) else (CE, y))

renderCommonEraDay :: CommonEraDay -> String
renderCommonEraDay (CommonEraDay y m d) =
  Printf.printf "%04d-%02d-%02d" y m d

commonEraDayToISO8601Day :: MonadFail m => CommonEra -> CommonEraDay -> m Time.Day
commonEraDayToISO8601Day ce (CommonEraDay y m d) =
  let
    iso8601Year = case ce of
      BCE -> 1 - y
      CE -> y
  in
    case Time.fromGregorianValid iso8601Year m d of
      Nothing -> fail "invalid date"
      Just valid -> pure valid
