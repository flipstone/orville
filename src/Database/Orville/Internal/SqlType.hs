module Database.Orville.Internal.SqlType
  ( SqlType(..)
  , serial
  , bigserial
  , text
  , varText
  , integer
  , bigInteger
  , double
  , boolean
  , date
  , timestamp
  , textSearchVector
  , convertSqlType
  , maybeConvertSqlType
  , nullableType
  , foreignRefType
  ) where

import Control.Monad ((<=<))
import Data.Convertible (safeConvert)
import qualified Data.Int as Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Time as Time
import qualified Database.HDBC as HDBC

data SqlType a = SqlType
  { sqlTypeDDL :: String
  , sqlTypeReferenceDDL :: Maybe String
  , sqlTypeNullable :: Bool
  , sqlTypeId :: HDBC.SqlTypeId
  , sqlTypeSqlSize :: Maybe Int
  , sqlTypeToSql :: a -> HDBC.SqlValue
  , sqlTypeFromSql :: HDBC.SqlValue -> Maybe a
  }

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

maybeConvertSqlType :: (b -> a) -> (a -> Maybe b) -> SqlType a -> SqlType b
maybeConvertSqlType bToA aToB sqlType =
  sqlType
    { sqlTypeToSql = sqlTypeToSql sqlType . bToA
    , sqlTypeFromSql = aToB <=< sqlTypeFromSql sqlType
    }

convertSqlType :: (b -> a) -> (a -> b) -> SqlType a -> SqlType b
convertSqlType bToA aToB = maybeConvertSqlType bToA (Just . aToB)

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

foreignRefType :: SqlType a -> SqlType a
foreignRefType sqlType =
  case sqlTypeReferenceDDL sqlType of
    Nothing -> sqlType
    Just refDDL -> sqlType {sqlTypeDDL = refDDL, sqlTypeReferenceDDL = Nothing}

int32ToSql :: Int.Int32 -> HDBC.SqlValue
int32ToSql = HDBC.SqlInt32

int32FromSql :: HDBC.SqlValue -> Maybe Int.Int32
int32FromSql sql =
  case sql of
    HDBC.SqlInt32 n -> Just n
    HDBC.SqlInteger n ->
      case safeConvert n of
        Left _ -> Nothing
        Right int32 -> Just int32
    _ -> Nothing

int64ToSql :: Int.Int64 -> HDBC.SqlValue
int64ToSql = HDBC.SqlInt64

int64FromSql :: HDBC.SqlValue -> Maybe Int.Int64
int64FromSql sql =
  case sql of
    HDBC.SqlInt64 n -> Just n
    HDBC.SqlInteger n ->
      case safeConvert n of
        Left _ -> Nothing
        Right int64 -> Just int64
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
    HDBC.SqlUTCTime t -> Just t
    _ -> Nothing
