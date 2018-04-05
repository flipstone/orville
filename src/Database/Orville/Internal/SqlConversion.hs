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
  ) where

import Control.Monad ((<=<))
import Data.Convertible

import Database.HDBC

data SqlConversion a = SqlConversion
  { convertToSql :: a -> SqlValue
  , convertFromSql :: SqlValue -> Maybe a
  }

sqlConversion :: (a -> SqlValue) -> (SqlValue -> Maybe a) -> SqlConversion a
sqlConversion = SqlConversion

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
