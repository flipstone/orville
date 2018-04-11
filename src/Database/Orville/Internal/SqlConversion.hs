{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.Internal.SqlConversion
  ( SqlConversion
  , sqlConversion
  , sqlConversionVia
  , maybeSqlConversionVia
  , sqlConvertible
  , convertToSql
  , convertFromSql
  , nullableConversion
  , textConversion
  , dayConversion
  , utcTimeConversion
  , intConversion
  , int32Conversion
  , int64Conversion
  , doubleConversion
  , boolConversion
  , uuidConversion
  ) where

import Control.Monad ((<=<))
import Data.Convertible
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Database.HDBC

data SqlConversion a = SqlConversion
  { convertToSql :: a -> SqlValue
  , convertFromSql :: SqlValue -> Maybe a
  }

sqlConversion :: (a -> SqlValue) -> (SqlValue -> Maybe a) -> SqlConversion a
sqlConversion = SqlConversion

textConversion :: SqlConversion Text
textConversion = sqlConvertible

dayConversion :: SqlConversion Day
dayConversion = sqlConvertible

utcTimeConversion :: SqlConversion UTCTime
utcTimeConversion = sqlConvertible

intConversion :: SqlConversion Int
intConversion = sqlConvertible

int32Conversion :: SqlConversion Int32
int32Conversion = sqlConvertible

int64Conversion :: SqlConversion Int64
int64Conversion = sqlConvertible

doubleConversion :: SqlConversion Double
doubleConversion = sqlConvertible

boolConversion :: SqlConversion Bool
boolConversion = sqlConvertible

uuidConversion :: SqlConversion UUID
uuidConversion =
  maybeSqlConversionVia (UUID.toText) (UUID.fromText) textConversion

nullableConversion :: SqlConversion a -> SqlConversion (Maybe a)
nullableConversion aConversion = sqlConversion maybeToSql maybeFromSql
  where
    maybeToSql = maybe SqlNull (convertToSql aConversion)
    maybeFromSql SqlNull = Just Nothing
    maybeFromSql sql = Just <$> convertFromSql aConversion sql

maybeSqlConversionVia ::
     (b -> a) -> (a -> Maybe b) -> SqlConversion a -> SqlConversion b
maybeSqlConversionVia bToA aToB aConversion =
  sqlConversion
    (convertToSql aConversion . bToA)
    (aToB <=< convertFromSql aConversion)

sqlConversionVia :: (b -> a) -> (a -> b) -> SqlConversion a -> SqlConversion b
sqlConversionVia bToA aToB = maybeSqlConversionVia bToA (Just . aToB)

sqlConvertible ::
     (Convertible a SqlValue, Convertible SqlValue a) => SqlConversion a
sqlConvertible = sqlConversion convert safeConvertFromSql
  where
    safeConvertFromSql sql =
      case safeConvert sql of
        Right a -> Just a
        Left _ -> Nothing
