module Orville.PostgreSQL.Internal.PgTime
  ( dayToPostgreSQL,
    dayFromPostgreSQL,
    utcTimeToPostgreSQL,
    utcTimeFromPostgreSQL,
    localTimeToPostgreSQL,
    localTimeFromPostgreSQL,
  )
where

import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as B8
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
  Parses a 'Time.Day' from a PostgreSQL textual representation. Returns
  'Nothing' if the parsing fails.
-}
dayFromPostgreSQL :: B8.ByteString -> Maybe Time.Day
dayFromPostgreSQL bytes = do
  txt <-
    case TextEnc.decodeUtf8' bytes of
      Right t -> Just t
      Left _ -> Nothing

  Time.parseTimeM
    False
    Time.defaultTimeLocale
    (Time.iso8601DateFormat Nothing)
    (T.unpack txt)

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
utcTimeFromPostgreSQL :: B8.ByteString -> Maybe Time.UTCTime
utcTimeFromPostgreSQL bytes = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  txt <-
    case TextEnc.decodeUtf8' bytes of
      Right t -> Just t
      Left _ -> Nothing

  let parseTime = Time.parseTimeM False Time.defaultTimeLocale
      unTxt = T.unpack txt

  parseTime "%F %T%Q%Z" (unTxt <> "00")
    <|> parseTime "%F %T%Z" (unTxt <> "00")

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
localTimeFromPostgreSQL :: B8.ByteString -> Maybe Time.LocalTime
localTimeFromPostgreSQL bytes = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  txt <-
    case TextEnc.decodeUtf8' bytes of
      Right t -> Just t
      Left _ -> Nothing

  let parseTime = Time.parseTimeM False Time.defaultTimeLocale
      unTxt = T.unpack txt

  parseTime "%F %T%Q" unTxt
    <|> parseTime "%F %T" unTxt
