module Orville.PostgreSQL.Internal.PgTime
  ( dayToPostgreSQL,
    day,
    utcTimeToPostgreSQL,
    utcTimeFromPostgreSQL,
    localTimeToPostgreSQL,
    localTimeFromPostgreSQL,
  )
where

import qualified Control.Exception as Exc
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time as Time

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
  y <- AttoB8.decimal <* AttoB8.char '-'
  m <- twoDigits <* AttoB8.char '-'
  d <- twoDigits
  maybe (fail "invalid date format") pure $ Time.fromGregorianValid y m d

{- |
  An Attoparsec parser for parsing 2 digit integers.
-}
twoDigits :: AttoB8.Parser Int
twoDigits = do
  tens <- AttoB8.digit
  ones <- AttoB8.digit
  pure $ fromChar tens * 10 + fromChar ones
  where
    fromChar c = Char.ord c - Char.ord '0'

{- |
  Renders a 'Time.UTCTime' value to a textual representation for PostgreSQL
-}
utcTimeToPostgreSQL :: Time.UTCTime -> B8.ByteString
utcTimeToPostgreSQL =
  B8.pack . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S+00"

{- |
  Parses a 'Time.UTCTime' from a PostgreSQL textual representation. Returns
  'Nothing' if the parsing fails.
-}
utcTimeFromPostgreSQL :: B8.ByteString -> Either String Time.UTCTime
utcTimeFromPostgreSQL bytes = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  string <- utf8BytesToString bytes

  let stringWithOffsetPad = string <> "00"

  firstThenTry
    (decodeTime "%F %T%Q%Z" stringWithOffsetPad)
    (decodeTime "%F %T%Z" stringWithOffsetPad)

{- |
  Renders a 'Time.LocalTime value to a textual representation for PostgreSQL
-}
localTimeToPostgreSQL :: Time.LocalTime -> B8.ByteString
localTimeToPostgreSQL =
  B8.pack . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%d %H:%M:%S"

{- |
  Parses a 'Time.LocalTime' from a PostgreSQL textual representation. Returns
  'Nothing' if the parsing fails.
-}
localTimeFromPostgreSQL :: B8.ByteString -> Either String Time.LocalTime
localTimeFromPostgreSQL bytes = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  string <- utf8BytesToString bytes

  firstThenTry
    (decodeTime "%F %T%Q" string)
    (decodeTime "%F %T" string)

firstThenTry :: Either String a -> Either String a -> Either String a
firstThenTry first thenTry =
  case first of
    Right _ -> first
    Left _ -> thenTry

utf8BytesToString :: B8.ByteString -> Either String String
utf8BytesToString bytes =
  case TextEnc.decodeUtf8' bytes of
    Right t -> Right (T.unpack t)
    Left err -> Left (Exc.displayException err)

decodeTime :: Time.ParseTime a => String -> String -> Either String a
decodeTime format string =
  case Time.parseTimeM False Time.defaultTimeLocale format string of
    Just t ->
      Right t
    Nothing ->
      Left $ "Unable to decode time value in format " <> show format
