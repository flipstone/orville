{- |

Module    : Orville.PostgreSQL.Internal.SqlValue
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT

The funtions in this module are named with the intent that it is imported
qualified as 'SqlValue.
-}
module Orville.PostgreSQL.Internal.SqlValue
  ( SqlValue,
    isSqlNull,
    sqlNull,
    fromInt32,
    toInt32,
    fromInt64,
    toInt64,
    fromInt,
    toInt,
    fromDouble,
    toDouble,
    fromBool,
    toBool,
    fromText,
    toText,
    fromDay,
    toDay,
    fromUTCTime,
    toUTCTimeWithZone,
    toUTCTimeWithoutZone,
    fromRawBytes,
    fromRawBytesNullable,
    toPGValue,
  )
where

import qualified Data.Attoparsec.ByteString as AttoBS
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time as Time

import Orville.PostgreSQL.Internal.PGTextFormatValue (PGTextFormatValue)
import qualified Orville.PostgreSQL.Internal.PGTextFormatValue as PGTextFormatValue

import Control.Applicative ((<|>))

data SqlValue
  = SqlValue PGTextFormatValue
  | SqlNull

{- |
  Checks whether the 'SqlValue' represents a sql NULL value in the database.
-}
isSqlNull :: SqlValue -> Bool
isSqlNull sqlValue =
  case sqlValue of
    SqlValue _ -> False
    SqlNull -> True

{- |
  A value of 'SqlValue' that will be interpreted as a sql NULL value when
  pasesed to the database.
-}
sqlNull :: SqlValue
sqlNull =
  SqlNull

{- |
  Converts a 'SqlValue' to its underlying raw bytes as it will be represented
  when sent to the database. The output should be recognizable as similar to
  to values you would write in query. If the value represents a sql NULL
  value, 'Nothing' is returned
-}
toPGValue :: SqlValue -> Maybe PGTextFormatValue
toPGValue sqlValue =
  case sqlValue of
    SqlValue value ->
      Just value
    SqlNull ->
      Nothing

{- |
  Creates a 'SqlValue' from a raw byte string as if the bytes had returned
  by the database. This function does not interpret the bytes in any way,
  but the using decode functions on them might fail depending on whether the
  bytes can be parsed as the requested type.

  Note: A value to present a sql NULL cannot be constructed using this
  function. See 'fromRawBytesNullable' for how to represent a nullable
  raw value.
-}
fromRawBytes :: BS.ByteString -> SqlValue
fromRawBytes =
  SqlValue . PGTextFormatValue.fromByteString

{- |
  Creates a 'SqlValue' from a raw byte string. If 'Nothing' is specified as the
  input parameter then the resulting 'SqlValue' will represent a NULL value in
  sql. Otherwise the bytes given are used in the same way as 'fromRawBytes'
-}
fromRawBytesNullable :: Maybe BS.ByteString -> SqlValue
fromRawBytesNullable =
  maybe sqlNull fromRawBytes

{- |
  Encodes an 'Int32' value for usage with database
-}
fromInt32 :: Int32 -> SqlValue
fromInt32 =
  fromBSBuilderWithNoNULs BSB.int32Dec

{- |
  Attempts to decode a 'SqlValue' a Haskell 'Int32' value. If decoding fails
  'Nothing' is returned.
-}
toInt32 :: SqlValue -> Maybe Int32
toInt32 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes an 'Int64' value for usage with database
-}
fromInt64 :: Int64 -> SqlValue
fromInt64 =
  fromBSBuilderWithNoNULs BSB.int64Dec

{- |
  Encodes an 'Int32' value for usage with database
-}
fromInt :: Int -> SqlValue
fromInt =
  fromBSBuilderWithNoNULs BSB.intDec

{- |
  Attempts to decode a 'SqlValue' a Haskell 'Int' value. If decoding fails
  'Nothing' is returned.
-}
toInt :: SqlValue -> Maybe Int
toInt =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Attempts to decode a 'SqlValue' a Haskell 'Int' value. If decoding fails
  'Nothing' is returned.
-}
toInt64 :: SqlValue -> Maybe Int64
toInt64 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Double' value for usage with database
-}
fromDouble :: Double -> SqlValue
fromDouble =
  fromBSBuilderWithNoNULs BSB.doubleDec

{- |
  Attempts to decode a 'SqlValue' a Haskell 'Double' value. If decoding fails
  'Nothing' is returned.
-}
toDouble :: SqlValue -> Maybe Double
toDouble =
  toParsedValue (AttoB8.signed AttoB8.double)

{- |
  Encodes a 'Bool' value for usage with database
-}
fromBool :: Bool -> SqlValue
fromBool =
  fromBSBuilderWithNoNULs $ \bool ->
    case bool of
      True -> BSB.char8 't'
      False -> BSB.char8 'f'

{- |
  Attempts to decode a 'SqlValue' a Haskell 'Bool' value. If decoding fails
  'Nothing' is returned.
-}
toBool :: SqlValue -> Maybe Bool
toBool =
  toParsedValue $ do
    char <- AttoB8.anyChar
    case char of
      't' -> pure True
      'f' -> pure False
      _ -> fail "Invalid boolean character value"

{- |
  Encodes a 'T.Text' value as utf8 so that it can be used with the database.
-}
fromText :: T.Text -> SqlValue
fromText =
  SqlValue . PGTextFormatValue.fromByteString . TextEnc.encodeUtf8

{- |
  Attempts to decode a 'SqlValue' as UTF-8 text. If the decoding fails,
  'Nothing' is returned.

  Note: This decoding _only_ fails if the bytes returned from the database
  are not a value UTF-8 sequence of bytes. Otherwise it always succeeds.
-}
toText :: SqlValue -> Maybe T.Text
toText =
  toBytesValue $ \bytes ->
    case TextEnc.decodeUtf8' bytes of
      Right t -> Just t
      Left _ -> Nothing

{- |
  Encodes a 'Time.Day' value as text in YYYY-MM-DD format so that it can be
  used with the database.
-}
fromDay :: Time.Day -> SqlValue
fromDay =
  SqlValue . PGTextFormatValue.unsafeFromByteString . B8.pack . Time.showGregorian

{- |
  Attempts to decode a 'SqlValue' as into a 'Time.Day' value by parsing it
  from YYYY-MM-DD format. If the decoding fails 'Nothing' is returned.
-}
toDay :: SqlValue -> Maybe Time.Day
toDay sqlValue = do
  txt <- toText sqlValue
  Time.parseTimeM
    False
    Time.defaultTimeLocale
    (Time.iso8601DateFormat Nothing)
    (T.unpack txt)

{- |
  Encodes a 'Time.UTCTime' in ISO 8601 format for usage with the database.
-}
fromUTCTime :: Time.UTCTime -> SqlValue
fromUTCTime =
  SqlValue
    . PGTextFormatValue.unsafeFromByteString
    . B8.pack
    . Time.formatTime Time.defaultTimeLocale "%0Y-%m-%dT%H:%M:%S"

{- |
  Attempts to decode a 'SqlValue' as a 'Time.UTCTime' formatted in iso8601
  format without time zone. If the decoding fails, 'Nothing' is returned.
-}
toUTCTimeWithoutZone :: SqlValue -> Maybe Time.UTCTime
toUTCTimeWithoutZone sqlValue = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  txt <- toText sqlValue
  let parseTime = Time.parseTimeM False Time.defaultTimeLocale
      unTxt = T.unpack txt

  parseTime "%F %T%Q" unTxt
    <|> parseTime "%F %T" unTxt

{- |
  Attempts to decode a 'SqlValue' as a 'Time.UTCTime' formatted in iso8601
  format with time zone. If the decoding fails, 'Nothing' is returned.
-}
toUTCTimeWithZone :: SqlValue -> Maybe Time.UTCTime
toUTCTimeWithZone sqlValue = do
  -- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
  -- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
  -- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
  -- Leading to the ugly hacks below.
  txt <- toText sqlValue
  let parseTime = Time.parseTimeM False Time.defaultTimeLocale
      unTxt = T.unpack txt

  parseTime "%F %T%Q%Z" (unTxt <> "00")
    <|> parseTime "%F %T%Z" (unTxt <> "00")

{- |
  A internal helper function that constructs a 'SqlValue' via a byte string builder
-}
fromBSBuilderWithNoNULs :: (a -> BSB.Builder) -> a -> SqlValue
fromBSBuilderWithNoNULs builder =
  SqlValue
    . PGTextFormatValue.unsafeFromByteString
    . LBS.toStrict
    . BSB.toLazyByteString
    . builder

{- |
  A internal helper function that parses 'SqlValue' via an Attoparsec parser.
-}
toParsedValue :: AttoB8.Parser a -> SqlValue -> Maybe a
toParsedValue parser =
  toBytesValue $ \bytes ->
    case AttoBS.parseOnly parser bytes of
      Left _ -> Nothing
      Right i -> Just i

{- |
  An internal helper function that parses the bytes from a 'SqlValue'
  with the given parsing function. If the 'SqlValue' is NULL, 'Nothing'
  is returned. If the parsing function fails (by returning 'Nothing'), then
  'Nothing' is returned from this function as well.
-}
toBytesValue :: (BS.ByteString -> Maybe a) -> SqlValue -> Maybe a
toBytesValue byteParser sqlValue =
  case sqlValue of
    SqlNull ->
      Nothing
    SqlValue bytes ->
      byteParser (PGTextFormatValue.toByteString bytes)
