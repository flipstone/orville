module Database.Orville.PostgreSQL.Internal.SqlType
  ( SqlType(..)
  , serial
  , bigserial
  , text
  , unboundedText
  , varText
  , integer
  , bigInteger
  , double
  , boolean
  , date
  , timestamp
  , textSearchVector
  , nullableType
  , foreignRefType
  , convertSqlType
  , maybeConvertSqlType
  ) where

import Control.Monad ((<=<))
import qualified Data.Int as Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Time as Time
import qualified Database.HDBC as HDBC

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
  , sqlTypeId :: HDBC.SqlTypeId
    -- ^ 'sqlTypeId' will be compared to the 'colType' field found in the
    -- 'HDBC.SqlColDesc' return by 'describeTable' when determining whether
    -- a column type change is required when migrating the database.
  , sqlTypeSqlSize :: Maybe Int
    -- ^ 'sqlTypeSqlSize will be compared to the 'colSize' field found in the
    -- 'HDBC.SqlColDesc' return by 'describeTable' when determining whether
    -- a column type change is required when migrating the database.
  , sqlTypeToSql :: a -> HDBC.SqlValue
    -- ^ A function for converting Haskell values of this type into values to
    -- be stored in the database.
  , sqlTypeFromSql :: HDBC.SqlValue -> Maybe a
    -- ^ A function for converting values of this are stored in the database
    -- into Haskell values. This function should return 'Nothing' to indicate
    -- an error if the conversion is impossible. Otherwise it should return
    -- 'Just' the corresponding 'a' value.
  }

{-|
  'serial' defines a 32-bit auto-incrementing column type. This corresponds to
  the "SERIAL" type in PostgreSQL.
  -}
serial :: SqlType Int.Int32
serial =
  SqlType
    { sqlTypeDDL = "SERIAL"
    , sqlTypeReferenceDDL = Just "INTEGER"
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlBigIntT
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = int32ToSql
    , sqlTypeFromSql = int32FromSql
    }

{-|
  'bigserial' defines a 64-bit auto-incrementing column type. This corresponds to
  the "BIGSERIAL" type in PostgresSQL.
  -}
bigserial :: SqlType Int.Int64
bigserial =
  SqlType
    { sqlTypeDDL = "BIGSERIAL"
    , sqlTypeReferenceDDL = Just "BIGINT"
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlBigIntT
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = int64ToSql
    , sqlTypeFromSql = int64FromSql
    }

{-|
  'text' defines a fixed length text field type. This corresponds to a
  "CHAR(len)" type in SQL.
  -}
text :: Int -> SqlType T.Text
text len =
  SqlType
    { sqlTypeDDL = concat ["CHAR(", show len, ")"]
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlCharT
    , sqlTypeSqlSize = Just len
    , sqlTypeToSql = textToSql
    , sqlTypeFromSql = textFromSql
    }

{-|
  'varText' defines a variable text field type with a max length. This
  corresponds to a "VARCHAR(len)" type in SQL.
  -}
varText :: Int -> SqlType T.Text
varText len =
  SqlType
    { sqlTypeDDL = concat ["VARCHAR(", show len, ")"]
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlVarCharT
    , sqlTypeSqlSize = Just len
    , sqlTypeToSql = textToSql
    , sqlTypeFromSql = textFromSql
    }

{-|
  'unboundedText' defines a fixed length text field type. This corresponds to a
  "TEXT" type in PostgreSQL.
  -}
unboundedText :: SqlType T.Text
unboundedText =
  SqlType
    { sqlTypeDDL = concat ["TEXT"]
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlCharT
    , sqlTypeSqlSize = Nothing
    , sqlTypeToSql = textToSql
    , sqlTypeFromSql = textFromSql
    }


{-|
  'integer' defines a 32-bit integer type. This corresponds to the "INTEGER" type in SQL.
  -}
integer :: SqlType Int.Int32
integer =
  SqlType
    { sqlTypeDDL = "INTEGER"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlBigIntT
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = int32ToSql
    , sqlTypeFromSql = int32FromSql
    }

{-|
  'bigInteger' defines a 64-bit integer type. This corresponds to the "BIGINT"
  type in SQL.
  -}
bigInteger :: SqlType Int.Int64
bigInteger =
  SqlType
    { sqlTypeDDL = "BIGINT"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlBigIntT
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = int64ToSql
    , sqlTypeFromSql = int64FromSql
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
    , sqlTypeId = HDBC.SqlFloatT
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = doubleToSql
    , sqlTypeFromSql = doubleFromSql
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
    , sqlTypeId = HDBC.SqlBitT
    , sqlTypeSqlSize = Just 1
    , sqlTypeToSql = booleanToSql
    , sqlTypeFromSql = booleanFromSql
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
    , sqlTypeId = HDBC.SqlDateT
    , sqlTypeSqlSize = Just 4
    , sqlTypeToSql = dayToSql
    , sqlTypeFromSql = dayFromSql
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
    , sqlTypeId = HDBC.SqlTimestampWithZoneT
    , sqlTypeSqlSize = Just 8
    , sqlTypeToSql = utcTimeToSql
    , sqlTypeFromSql = utcTimeFromSql
    }

{-|
  'textSearchVector' defines a type for indexed text searching. It corresponds to the
  "TSVECTOR" type in PostgreSQL.
  -}
textSearchVector :: SqlType T.Text
textSearchVector =
  SqlType
    { sqlTypeDDL = "TSVECTOR"
    , sqlTypeReferenceDDL = Nothing
    , sqlTypeNullable = False
    , sqlTypeId = HDBC.SqlUnknownT "3614"
    , sqlTypeSqlSize = Nothing
    , sqlTypeToSql = textToSql
    , sqlTypeFromSql = textFromSql
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
    , sqlTypeToSql = maybe HDBC.SqlNull (sqlTypeToSql sqlType)
    , sqlTypeFromSql =
        \sql ->
          case sql of
            HDBC.SqlNull -> Just Nothing
            _ -> Just <$> sqlTypeFromSql sqlType sql
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
    , sqlTypeFromSql = aToB <=< sqlTypeFromSql sqlType
    }

{-|
  'convertSqlType' changes the Haskell type used by a 'SqlType' in the same manner
  as 'maybeConvertSqlType' in cases where an 'a' can always be converted to a 'b'.
  -}
convertSqlType :: (b -> a) -> (a -> b) -> SqlType a -> SqlType b
convertSqlType bToA aToB = maybeConvertSqlType bToA (Just . aToB)

int32ToSql :: Int.Int32 -> HDBC.SqlValue
int32ToSql = HDBC.SqlInt32

int32FromSql :: HDBC.SqlValue -> Maybe Int.Int32
int32FromSql sql =
  case sql of
    HDBC.SqlInt32 n -> Just n
    HDBC.SqlInteger n -> toBoundedInteger n
    _ -> Nothing

int64ToSql :: Int.Int64 -> HDBC.SqlValue
int64ToSql = HDBC.SqlInt64

int64FromSql :: HDBC.SqlValue -> Maybe Int.Int64
int64FromSql sql =
  case sql of
    HDBC.SqlInt64 n -> Just n
    HDBC.SqlInteger n -> toBoundedInteger n
    _ -> Nothing

textToSql :: T.Text -> HDBC.SqlValue
textToSql = HDBC.SqlByteString . Enc.encodeUtf8

textFromSql :: HDBC.SqlValue -> Maybe T.Text
textFromSql sql =
  case sql of
    HDBC.SqlByteString bytes -> Just $ Enc.decodeUtf8 bytes
    HDBC.SqlString string -> Just $ T.pack string
    _ -> Nothing

doubleToSql :: Double -> HDBC.SqlValue
doubleToSql = HDBC.SqlDouble

doubleFromSql :: HDBC.SqlValue -> Maybe Double
doubleFromSql sql =
  case sql of
    HDBC.SqlDouble d -> Just d
    _ -> Nothing

booleanToSql :: Bool -> HDBC.SqlValue
booleanToSql = HDBC.SqlBool

booleanFromSql :: HDBC.SqlValue -> Maybe Bool
booleanFromSql sql =
  case sql of
    HDBC.SqlBool b -> Just b
    _ -> Nothing

dayToSql :: Time.Day -> HDBC.SqlValue
dayToSql = HDBC.SqlLocalDate

dayFromSql :: HDBC.SqlValue -> Maybe Time.Day
dayFromSql sql =
  case sql of
    HDBC.SqlLocalDate d -> Just d
    _ -> Nothing

utcTimeToSql :: Time.UTCTime -> HDBC.SqlValue
utcTimeToSql = HDBC.SqlUTCTime

utcTimeFromSql :: HDBC.SqlValue -> Maybe Time.UTCTime
utcTimeFromSql sql =
  case sql of
    HDBC.SqlUTCTime   t -> Just t
    HDBC.SqlZonedTime t -> Just (Time.zonedTimeToUTC t)
    _ -> Nothing

toBoundedInteger :: (Bounded num, Integral num) => Integer -> Maybe num
toBoundedInteger source =
  let truncated = fromInteger source
      upper = toInteger (maxBound `asTypeOf` truncated)
      lower = toInteger (minBound `asTypeOf` truncated)
   in if lower <= source && source <= upper
        then Just truncated
        else Nothing
