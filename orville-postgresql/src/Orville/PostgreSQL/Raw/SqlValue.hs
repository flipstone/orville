{- |

Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

The functions in this module are named with the intent that it is imported
qualified as 'SqlValue'.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.SqlValue
  ( SqlValue
  , isSqlNull
  , sqlNull
  , fromInt8
  , toInt8
  , fromInt16
  , toInt16
  , fromInt32
  , toInt32
  , fromInt64
  , toInt64
  , fromInt
  , toInt
  , fromWord8
  , toWord8
  , fromWord16
  , toWord16
  , fromWord32
  , toWord32
  , fromWord64
  , toWord64
  , fromWord
  , toWord
  , fromDouble
  , toDouble
  , fromBool
  , toBool
  , fromText
  , toText
  , fromDay
  , toDay
  , fromUTCTime
  , toUTCTime
  , fromLocalTime
  , toLocalTime
  , fromRawBytes
  , fromRawBytesNullable
  , toPgValue
  )
where

import qualified Control.Exception as Exc
import qualified Data.Attoparsec.ByteString as AttoBS
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TextEnc
import qualified Data.Time as Time
import qualified Data.Typeable as Typeable
import Data.Word (Word16, Word32, Word64, Word8)

import Orville.PostgreSQL.Raw.PgTextFormatValue (PgTextFormatValue)
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.PgTime as PgTime

{- |
  'SqlValue' represents a value that is in encoded format for use with LibPQ.
  It is used both for values passed to LibPQ and values parsed from LibPQ. The
  conversion functions in "Orville.PostgreSQL.Raw.SqlValue" can be used to
  convert to and from the value.

@since 1.0.0.0
-}
data SqlValue
  = SqlValue PgTextFormatValue
  | SqlNull
  deriving (Eq)

{- |
  Checks whether the 'SqlValue' represents a SQL NULL value in the database.

@since 1.0.0.0
-}
isSqlNull :: SqlValue -> Bool
isSqlNull sqlValue =
  case sqlValue of
    SqlValue _ -> False
    SqlNull -> True

{- |
  A value of 'SqlValue' that will be interpreted as a SQL NULL value when
  passed to the database.

@since 1.0.0.0
-}
sqlNull :: SqlValue
sqlNull =
  SqlNull

{- |
  Converts a 'SqlValue' to its underlying raw bytes as it will be represented
  when sent to the database. The output should be recognizable as similar to
  values you would write in a query. If the value represents a SQL NULL value,
  'Nothing' is returned.

@since 1.0.0.0
-}
toPgValue :: SqlValue -> Maybe PgTextFormatValue
toPgValue sqlValue =
  case sqlValue of
    SqlValue value ->
      Just value
    SqlNull ->
      Nothing

{- |
  Creates a 'SqlValue' from a raw bytestring as if the bytes had been returned
  by the database. This function does not interpret the bytes in any way, but
  using decode functions on them might fail depending on whether the bytes can
  be parsed as the requested type.

  Note: A value to represent a SQL NULL cannot be constructed using this
  function. See 'fromRawBytesNullable' for how to represent a nullable
  raw value.

  Warning: Will throw NULByteFoundError if there is a NULL byte. For arbitrary
           binary data see:
           https://www.postgresql.org/docs/current/datatype-binary.html#DATATYPE-BINARY-BYTEA-HEX-FORMAT

@since 1.0.0.0
-}
fromRawBytes :: BS.ByteString -> SqlValue
fromRawBytes =
  SqlValue . PgTextFormatValue.fromByteString

{- |
  Creates a 'SqlValue' from a raw bytestring. If 'Nothing' is specified as the
  input parameter then the resulting 'SqlValue' will represent a NULL value in
  SQL. Otherwise, the bytes given are used in the same way as 'fromRawBytes'.

@since 1.0.0.0
-}
fromRawBytesNullable :: Maybe BS.ByteString -> SqlValue
fromRawBytesNullable =
  maybe sqlNull fromRawBytes

{- |
  Encodes an 'Int8' value for use with the database.

@since 1.0.0.0
-}
fromInt8 :: Int8 -> SqlValue
fromInt8 =
  fromBSBuilderWithNoNULs BSB.int8Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Int8' value. If decoding fails,
  'Nothing' is returned.

@since 1.0.0.0
-}
toInt8 :: SqlValue -> Either String Int8
toInt8 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes an 'Int16' value for use with the database.

@since 1.0.0.0
-}
fromInt16 :: Int16 -> SqlValue
fromInt16 =
  fromBSBuilderWithNoNULs BSB.int16Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Int16' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toInt16 :: SqlValue -> Either String Int16
toInt16 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes an 'Int32' value for use with the database.

@since 1.0.0.0
-}
fromInt32 :: Int32 -> SqlValue
fromInt32 =
  fromBSBuilderWithNoNULs BSB.int32Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Int32' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toInt32 :: SqlValue -> Either String Int32
toInt32 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes an 'Int64' value for use with the database.

@since 1.0.0.0
-}
fromInt64 :: Int64 -> SqlValue
fromInt64 =
  fromBSBuilderWithNoNULs BSB.int64Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Int' value. If decoding fails,
  'Nothing' is returned.

@since 1.0.0.0
-}
toInt64 :: SqlValue -> Either String Int64
toInt64 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes an 'Int' value for use with the database.

@since 1.0.0.0
-}
fromInt :: Int -> SqlValue
fromInt =
  fromBSBuilderWithNoNULs BSB.intDec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Int' value. If decoding fails,
  'Nothing' is returned.

@since 1.0.0.0
-}
toInt :: SqlValue -> Either String Int
toInt =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Word8' value for use with the database.

@since 1.0.0.0
-}
fromWord8 :: Word8 -> SqlValue
fromWord8 =
  fromBSBuilderWithNoNULs BSB.word8Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Word8' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toWord8 :: SqlValue -> Either String Word8
toWord8 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Word16' value for use with the database.

@since 1.0.0.0
-}
fromWord16 :: Word16 -> SqlValue
fromWord16 =
  fromBSBuilderWithNoNULs BSB.word16Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Word16' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toWord16 :: SqlValue -> Either String Word16
toWord16 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Word32' value for use with the database.

@since 1.0.0.0
-}
fromWord32 :: Word32 -> SqlValue
fromWord32 =
  fromBSBuilderWithNoNULs BSB.word32Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Word32' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toWord32 :: SqlValue -> Either String Word32
toWord32 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Word64' value for use with the database.

@since 1.0.0.0
-}
fromWord64 :: Word64 -> SqlValue
fromWord64 =
  fromBSBuilderWithNoNULs BSB.word64Dec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Word64' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toWord64 :: SqlValue -> Either String Word64
toWord64 =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Word' value for use with the database.

@since 1.0.0.0
-}
fromWord :: Word -> SqlValue
fromWord =
  fromBSBuilderWithNoNULs BSB.wordDec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Word' value. If decoding fails,
  'Nothing' is returned.

@since 1.0.0.0
-}
toWord :: SqlValue -> Either String Word
toWord =
  toParsedValue (AttoB8.signed AttoB8.decimal)

{- |
  Encodes a 'Double' value for use with the database.

@since 1.0.0.0
-}
fromDouble :: Double -> SqlValue
fromDouble =
  fromBSBuilderWithNoNULs BSB.doubleDec

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Double' value. If decoding
  fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toDouble :: SqlValue -> Either String Double
toDouble =
  toParsedValue (AttoB8.signed AttoB8.double)

{- |
  Encodes a 'Bool' value for use with the database.

@since 1.0.0.0
-}
fromBool :: Bool -> SqlValue
fromBool =
  fromBSBuilderWithNoNULs $ \bool ->
    case bool of
      True -> BSB.char8 't'
      False -> BSB.char8 'f'

{- |
  Attempts to decode a 'SqlValue' as a Haskell 'Bool' value. If decoding fails,
  'Nothing' is returned.

@since 1.0.0.0
-}
toBool :: SqlValue -> Either String Bool
toBool =
  toParsedValue $ do
    char <- AttoB8.anyChar
    case char of
      't' -> pure True
      'f' -> pure False
      _ -> fail "Invalid boolean character value"

{- |
  Encodes a 'T.Text' value as UTF-8 so that it can be used with the database.

@since 1.0.0.0
-}
fromText :: T.Text -> SqlValue
fromText =
  SqlValue . PgTextFormatValue.fromByteString . TextEnc.encodeUtf8

{- |
  Attempts to decode a 'SqlValue' as UTF-8 text. If the decoding fails,
  'Nothing' is returned.

  Note: This decoding _only_ fails if the bytes returned from the database
  are not a valid UTF-8 sequence of bytes. Otherwise it always succeeds.

@since 1.0.0.0
-}
toText :: SqlValue -> Either String T.Text
toText =
  toBytesValue $ \bytes ->
    case TextEnc.decodeUtf8' bytes of
      Right t -> Right t
      Left err -> Left $ Exc.displayException err

{- |
  Encodes a 'Time.Day' value as text in YYYY-MM-DD format so that it can be
  used with the database.

@since 1.0.0.0
-}
fromDay :: Time.Day -> SqlValue
fromDay =
  SqlValue . PgTextFormatValue.unsafeFromByteString . PgTime.dayToPostgreSQL

{- |
  Attempts to decode a 'SqlValue' as into a 'Time.Day' value by parsing it
  from YYYY-MM-DD format. If the decoding fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toDay :: SqlValue -> Either String Time.Day
toDay =
  toParsedValue PgTime.day

{- |
  Encodes a 'Time.UTCTime' in ISO-8601 format for use with the database.

@since 1.0.0.0
-}
fromUTCTime :: Time.UTCTime -> SqlValue
fromUTCTime =
  SqlValue
    . PgTextFormatValue.unsafeFromByteString
    . PgTime.utcTimeToPostgreSQL

{- |
  Encodes a 'Time.LocalTime' in ISO-8601 format for use with the database.

@since 1.0.0.0
-}
fromLocalTime :: Time.LocalTime -> SqlValue
fromLocalTime =
  SqlValue
    . PgTextFormatValue.unsafeFromByteString
    . PgTime.localTimeToPostgreSQL

{- |
  Attempts to decode a 'SqlValue' as a 'Time.LocalTime' formatted in ISO-8601
  format in the default locale. If the decoding fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toLocalTime :: SqlValue -> Either String Time.LocalTime
toLocalTime =
  toParsedValue PgTime.localTime

{- |
  Attempts to decode a 'SqlValue' as a 'Time.UTCTime' formatted in ISO-8601
  format with time zone. If the decoding fails, 'Nothing' is returned.

@since 1.0.0.0
-}
toUTCTime :: SqlValue -> Either String Time.UTCTime
toUTCTime =
  toParsedValue PgTime.utcTime

{- |
  An internal helper function that constructs a 'SqlValue' via a bytestring
  'BS8.Builder'.

@since 1.0.0.0
-}
fromBSBuilderWithNoNULs :: (a -> BSB.Builder) -> a -> SqlValue
fromBSBuilderWithNoNULs builder =
  SqlValue
    . PgTextFormatValue.unsafeFromByteString
    . LBS.toStrict
    . BSB.toLazyByteString
    . builder

{- |
  An internal helper function that parses 'SqlValue' via an Attoparsec parser.

@since 1.0.0.0
-}
toParsedValue ::
  Typeable.Typeable a =>
  AttoB8.Parser a ->
  SqlValue ->
  Either String a
toParsedValue parser =
  toBytesValue (AttoBS.parseOnly parser)

{- |
  An internal helper function that parses the bytes from a 'SqlValue' with the
  given parsing function. If the 'SqlValue' is NULL, this function returns an
  error an a 'Left' value. If the parsing function fails (by returning 'Left'),
  the error it returns in returned by this function.

@since 1.0.0.0
-}
toBytesValue ::
  Typeable.Typeable a =>
  (BS.ByteString -> Either String a) ->
  SqlValue ->
  Either String a
toBytesValue byteParser sqlValue =
  let
    result =
      case sqlValue of
        SqlNull ->
          Left "Unexpected SQL NULL value"
        SqlValue bytes ->
          byteParser (PgTextFormatValue.toByteString bytes)

    typeRepOfA =
      -- result is the 'proxy a' for typeRep
      Typeable.typeRep result
  in
    case result of
      Right _ ->
        result
      Left err ->
        Left $ "Failed to decode PostgreSQL value as " <> Typeable.showsTypeRep typeRepOfA (": " <> err)
