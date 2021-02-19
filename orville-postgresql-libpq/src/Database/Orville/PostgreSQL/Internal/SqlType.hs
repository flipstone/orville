{-|
Module    : Database.Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.SqlType
  (SqlType ( SqlType
           , sqlTypeDDL
           , sqlTypeReferenceDDL
           , sqlTypeNullable
           , sqlTypeId
           , sqlTypeSqlSize
           , sqlTypeToSql
           , sqlTypeFromSql
           )

  -- numeric types
  , integer
  , serial
  , bigInteger
  , bigserial
  , double

  -- textual-ish types
  , boolean
  , unboundedText
  , fixedText
  , boundedText
  , textSearchVector

  -- date types
  , date
  , timestamp

  -- type conversions
  , nullableType
  , foreignRefType
  , convertSqlType
  , maybeConvertSqlType
  ) where

import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString.Char8 as AttoB8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Builder (char8, doubleDec, int64Dec, int32Dec, stringUtf8, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Int (Int32, Int64)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.Time as Time

{-|
  SqlType defines the mapping of a Haskell type (`a`) to a SQL column type in the
  database. This includes both how to convert the type to and from the raw values
  read from the database as well as the schema information required to create
  and migrate columns using the type.
  -}
data SqlType a = SqlType
  { sqlTypeDDL :: String
    -- ^ The raw SQL DDL to use when creating/migrating columns of this type
    -- (not including any NULL or NOT NULL declarations)
  , sqlTypeReferenceDDL :: Maybe String
    -- ^ The raw SQL DDL to use when creating/migrating columns with foreign
    -- keys to this type. This is used foreignRefType to build a new SqlType
    -- when making foreign key fields
  , sqlTypeNullable :: Bool
    -- ^ Indicates whether columns should be marked NULL or NOT NULL in the
    -- database schema. If this is 'True', then 'sqlTypeFromSql' should
    -- provide a handling of 'SqlNull' that returns an 'a', not 'Nothing'.
  , sqlTypeId :: LibPQ.Oid
  , sqlTypeSqlSize :: Maybe Int
  , sqlTypeToSql :: a -> ByteString
    -- ^ A function for converting Haskell values of this type into values to
    -- be stored in the database.
  , sqlTypeFromSql :: ByteString -> Maybe a
    -- ^ A function for converting values of this are stored in the database
    -- into Haskell values. This function should return 'Nothing' to indicate
    -- an error if the conversion is impossible. Otherwise it should return
    -- 'Just' the corresponding 'a' value.
  }

{-|
  'integer' defines a 32-bit integer type. This corresponds to the "INTEGER" type in SQL.
-}
integer :: SqlType Int32
integer =
  SqlType
    { sqlTypeDDL = "INTEGER"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 23
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = int32ToBS
    , sqlTypeFromSql = int32FromBS
    }

{-|
  'serial' defines a 32-bit auto-incrementing column type. This corresponds to
  the "SERIAL" type in PostgreSQL.
-}
serial :: SqlType Int32
serial =
  SqlType
    { sqlTypeDDL = "SERIAL"
    , sqlTypeReferenceDDL = Just "INTEGER"
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 23
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = int32ToBS
    , sqlTypeFromSql = int32FromBS
    }

{-|
  'bigInteger' defines a 64-bit integer type. This corresponds to the "BIGINT"
  type in SQL.
-}
bigInteger :: SqlType Int64
bigInteger =
  SqlType
    { sqlTypeDDL = "BIGINT"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 20
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = int64ToBS
    , sqlTypeFromSql = int64FromBS
    }

{-|
  'bigserial' defines a 64-bit auto-incrementing column type. This corresponds to
  the "BIGSERIAL" type in PostgresSQL.
-}
bigserial :: SqlType Int64
bigserial =
  SqlType
    { sqlTypeDDL = "BIGSERIAL"
    , sqlTypeReferenceDDL = Just "BIGINT"
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 20
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = int64ToBS
    , sqlTypeFromSql = int64FromBS
    }

{-|
  'double' defines a floating point numeric type. This corresponds to the "DOUBLE
  PRECISION" type in SQL.
-}
double :: SqlType Double
double =
  SqlType
    { sqlTypeDDL = "DOUBLE PRECISION"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 701
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = doubleToBS
    , sqlTypeFromSql = doubleFromBS
    }

{-|
  'boolean' defines a True/False boolean type. This corresponds to the "BOOLEAN"
  type in SQL.
-}
boolean :: SqlType Bool
boolean =
  SqlType
    { sqlTypeDDL = "BOOLEAN"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 16
    , sqlTypeSqlSize = Just 1
    , sqlTypeToSql = booleanToBS
    , sqlTypeFromSql = booleanFromBS
    }

{-|
  'unboundedText' defines a unbounded length text field type. This corresponds to a
  "TEXT" type in PostgreSQL.
-}
unboundedText :: SqlType Text
unboundedText =
  SqlType
    { sqlTypeDDL = "TEXT"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 25
    , sqlTypeSqlSize = Nothing
    , sqlTypeToSql = textToBS
    , sqlTypeFromSql = textFromBS
    }

{-|
  'fixedText' defines a fixed length text field type. This corresponds to a
  "CHAR(len)" type in PostgreSQL.
-}
fixedText :: Int -> SqlType Text
fixedText len =
  SqlType
    { sqlTypeDDL = "CHAR(" <> show len <> ")"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 1042
    , sqlTypeSqlSize = Just  len
    , sqlTypeToSql = textToBS
    , sqlTypeFromSql = textFromBS
    }

{-|
  'boundedText' defines a variable length text field type. This corresponds to a
  "VARCHAR(len)" type in PostgreSQL.
-}
boundedText :: Int -> SqlType Text
boundedText len =
  SqlType
    { sqlTypeDDL = "VARCHAR(" <> show len <> ")"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 1043
    , sqlTypeSqlSize = Just  len
    , sqlTypeToSql = textToBS
    , sqlTypeFromSql = textFromBS
    }

{-|
  'textSearchVector' defines a type for indexed text searching. It corresponds to the
  "TSVECTOR" type in PostgreSQL.
-}
textSearchVector :: SqlType Text
textSearchVector =
  SqlType
    { sqlTypeDDL = "TSVECTOR"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 3614
    , sqlTypeSqlSize = Nothing
    , sqlTypeToSql = textToBS
    , sqlTypeFromSql = textFromBS
    }

{-|
  'date' defines a type representing a calendar date (without time zone). It corresponds
  to the "DATE" type in SQL.
-}
date :: SqlType Time.Day
date =
  SqlType
    { sqlTypeDDL = "DATE"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 1082
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = dayToBS
    , sqlTypeFromSql = dayFromBS
    }

{-|
  'timestamp' defines a type representing a particular point in time (without time zone).
  It corresponds to the "TIMESTAMP with time zone" type in SQL.

  Note: This is NOT a typo. The "TIMESTAMP with time zone" type in SQL does not include
  any actual time zone information. For an excellent explanation of the complexities
  involving this type, please see Chris Clark's blog post about it:
  http://blog.untrod.com/2016/08/actually-understanding-timezones-in-postgresql.html
-}
timestamp :: SqlType Time.UTCTime
timestamp =
  SqlType
    { sqlTypeDDL = "TIMESTAMP with time zone"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = LibPQ.Oid 1184
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = utcTimeToBS
    , sqlTypeFromSql = utcTimeFromBS
    }

{-|
   'nullableType' creates a nullable version of an existing 'SqlType'. The underlying
   sql type will be the same as the original, but column will be created with a 'NULL'
   constraint instead a 'NOT NULL' constraint. The Haskell value 'Nothing' will be used
   represent NULL values when converting to and from sql.
-}
nullableType :: SqlType a -> SqlType (Maybe a)
nullableType sqlType =
  sqlType
    { sqlTypeNullable = True
    , sqlTypeToSql = maybe nullBS (sqlTypeToSql sqlType)
    , sqlTypeFromSql =
        \sql ->
          if sql == nullBS then
            Just Nothing
          else
            fmap Just (sqlTypeFromSql sqlType sql)
    }

{-|
  'foreignRefType' creates a 'SqlType' suitable for columns will be foreign
  keys referencing a column of the given 'SqlType'. For most types the
  underlying sql type with be identical, but for special types (such as
  autoincrementing primary keys), the type construted by 'foreignRefType' with
  have regular underlying sql type. Each 'SqlType' definition must specify any
  special handling required when creating foreign reference types by setting
  the 'sqlTypeReferenceDDL' field to an appropriate value.
-}
foreignRefType :: SqlType a -> SqlType a
foreignRefType sqlType =
  case sqlTypeReferenceDDL sqlType of
    Nothing -> sqlType
    Just refDDL -> sqlType {sqlTypeDDL = refDDL, sqlTypeReferenceDDL = Nothing}

{-|
  'maybeConvertSqlType' changes the Haskell type used by a 'SqlType' which changing
  the column type that will be used in the database schema. The functions given
  will be used to convert the now Haskell type to and from the original type when
  reading and writing values from the database. When reading an 'a' value from
  the database, the conversion function should produce 'Nothing' if the value
  cannot be successfully converted to a 'b'
-}
maybeConvertSqlType :: (b -> a) -> (a -> Maybe b) -> SqlType a -> SqlType b
maybeConvertSqlType bToA aToB sqlType =
  sqlType
    { sqlTypeToSql = sqlTypeToSql sqlType . bToA
    , sqlTypeFromSql = \sql -> do
        a <- sqlTypeFromSql sqlType sql
        aToB a
    }

{-|
  'convertSqlType' changes the Haskell type used by a 'SqlType' in the same manner
  as 'maybeConvertSqlType' in cases where an 'a' can always be converted to a 'b'.
-}
convertSqlType :: (b -> a) -> (a -> b) -> SqlType a -> SqlType b
convertSqlType bToA aToB =
  maybeConvertSqlType bToA (Just . aToB)


int32ToBS :: Int32 -> ByteString
int32ToBS =
  toStrict . toLazyByteString . int32Dec

int32FromBS :: ByteString -> Maybe Int32
int32FromBS bs =
  case parseOnly (AttoB8.signed AttoB8.decimal) bs of
    Left _ -> Nothing
    Right i -> Just i

int64ToBS :: Int64 -> ByteString
int64ToBS =
  toStrict . toLazyByteString . int64Dec

int64FromBS :: ByteString -> Maybe Int64
int64FromBS bs =
  case parseOnly (AttoB8.signed AttoB8.decimal) bs of
    Left _ -> Nothing
    Right i -> Just i

doubleToBS :: Double -> ByteString
doubleToBS =
  toStrict . toLazyByteString . doubleDec

doubleFromBS :: ByteString -> Maybe Double
doubleFromBS bs =
  case parseOnly (AttoB8.signed AttoB8.double) bs of
    Left _ -> Nothing
    Right i -> Just i

booleanToBS :: Bool -> ByteString
booleanToBS True =
  toStrict . toLazyByteString $ char8 't'
booleanToBS False =
  toStrict . toLazyByteString $ char8 'f'

booleanFromBS :: ByteString -> Maybe Bool
booleanFromBS bs =
  case parseOnly AttoB8.anyChar bs of
    Right 't' -> Just True
    Right 'f' -> Just False
    Right _ -> Nothing
    Left _ -> Nothing

textToBS :: Text -> ByteString
textToBS =
  encodeUtf8

textFromBS :: ByteString -> Maybe Text
textFromBS bs =
  case decodeUtf8' bs of
    Right t -> Just t
    Left _ -> Nothing

dayToBS :: Time.Day -> ByteString
dayToBS =
  B8.pack . Time.showGregorian

dayFromBS :: ByteString -> Maybe Time.Day
dayFromBS bs =
  do
    txt <- textFromBS bs
    Time.parseTimeM False Time.defaultTimeLocale (Time.iso8601DateFormat Nothing) (unpack txt)

utcTimeToBS :: Time.UTCTime -> ByteString
utcTimeToBS =
  B8.pack . Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat Nothing)

-- N.B. There are dragons here... Notably the iso8601DateFormat (at least as of time-1.9.x)
-- However PostgreSQL adheres to a different version of the standard which ommitted the 'T' and instead used a space.
-- Further... PostgreSQL uses the short format for the UTC offset and the haskell library does not support this.
-- Leading to the ugly hacks below.
utcTimeFromBS :: ByteString -> Maybe Time.UTCTime
utcTimeFromBS bs = do
  txt <- textFromBS bs
  Time.parseTimeM False Time.defaultTimeLocale "%F %T%Q%Z" (unpack txt <> "00")

-- | NULL as a bytestring
nullBS :: ByteString
nullBS =
  toStrict . toLazyByteString $ stringUtf8 "null"
