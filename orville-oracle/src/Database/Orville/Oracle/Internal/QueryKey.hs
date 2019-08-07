{-|
Module    : Database.Orville.Oracle.Internal.QueryKey
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.Oracle.Internal.QueryKey where

import Database.Orville.Oracle.Internal.MappendCompat ((<>))

import Data.Time.LocalTime
import Database.HDBC

data QueryKey
  = QKValue OrdSqlValue
  | QKField String
  | QKTable String
  | QKOp String
         QueryKey
  | QKList [QueryKey]
  | QKEmpty
  deriving (Eq, Ord)
#if MIN_VERSION_base(4,11,0)
instance Semigroup QueryKey where
  (<>) = appendQueryKeys
#endif
instance Monoid QueryKey where
  mempty = QKEmpty
  mappend = appendQueryKeys
  mconcat = QKList

appendQueryKeys :: QueryKey -> QueryKey -> QueryKey
appendQueryKeys a b = QKList [a, b]

class QueryKeyable a where
  queryKey :: a -> QueryKey

instance QueryKeyable QueryKey where
  queryKey = id

instance QueryKeyable a => QueryKeyable [a] where
  queryKey = foldMap queryKey

instance QueryKeyable a => QueryKeyable (Maybe a) where
  queryKey = foldMap queryKey

instance QueryKeyable SqlValue where
  queryKey = QKValue . OrdSqlValue

qkOp :: (QueryKeyable a) => String -> a -> QueryKey
qkOp op a = QKOp op $ queryKey a

qkOp2 :: (QueryKeyable a, QueryKeyable b) => String -> a -> b -> QueryKey
qkOp2 op a b = QKOp op $ QKList [queryKey a, queryKey b]

newtype OrdSqlValue =
  OrdSqlValue SqlValue

instance Ord OrdSqlValue where
  compare (OrdSqlValue s) (OrdSqlValue s') = compareSqlValue s s'

instance Eq OrdSqlValue where
  a == b = compare a b == EQ

compareSqlValue :: SqlValue -> SqlValue -> Ordering
compareSqlValue (SqlString v) (SqlString v') = compare v v'
compareSqlValue (SqlString _) _ = LT
compareSqlValue _ (SqlString _) = GT
compareSqlValue (SqlByteString v) (SqlByteString v') = compare v v'
compareSqlValue (SqlByteString _) _ = LT
compareSqlValue _ (SqlByteString _) = GT
compareSqlValue (SqlWord32 v) (SqlWord32 v') = compare v v'
compareSqlValue (SqlWord32 _) _ = LT
compareSqlValue _ (SqlWord32 _) = GT
compareSqlValue (SqlWord64 v) (SqlWord64 v') = compare v v'
compareSqlValue (SqlWord64 _) _ = LT
compareSqlValue _ (SqlWord64 _) = GT
compareSqlValue (SqlInt32 v) (SqlInt32 v') = compare v v'
compareSqlValue (SqlInt32 _) _ = LT
compareSqlValue _ (SqlInt32 _) = GT
compareSqlValue (SqlInt64 v) (SqlInt64 v') = compare v v'
compareSqlValue (SqlInt64 _) _ = LT
compareSqlValue _ (SqlInt64 _) = GT
compareSqlValue (SqlInteger v) (SqlInteger v') = compare v v'
compareSqlValue (SqlInteger _) _ = LT
compareSqlValue _ (SqlInteger _) = GT
compareSqlValue (SqlChar v) (SqlChar v') = compare v v'
compareSqlValue (SqlChar _) _ = LT
compareSqlValue _ (SqlChar _) = GT
compareSqlValue (SqlBool v) (SqlBool v') = compare v v'
compareSqlValue (SqlBool _) _ = LT
compareSqlValue _ (SqlBool _) = GT
compareSqlValue (SqlDouble v) (SqlDouble v') = compare v v'
compareSqlValue (SqlDouble _) _ = LT
compareSqlValue _ (SqlDouble _) = GT
compareSqlValue (SqlRational v) (SqlRational v') = compare v v'
compareSqlValue (SqlRational _) _ = LT
compareSqlValue _ (SqlRational _) = GT
compareSqlValue (SqlLocalDate v) (SqlLocalDate v') = compare v v'
compareSqlValue (SqlLocalDate _) _ = LT
compareSqlValue _ (SqlLocalDate _) = GT
compareSqlValue (SqlLocalTimeOfDay v) (SqlLocalTimeOfDay v') = compare v v'
compareSqlValue (SqlLocalTimeOfDay _) _ = LT
compareSqlValue _ (SqlLocalTimeOfDay _) = GT
compareSqlValue (SqlZonedLocalTimeOfDay v z) (SqlZonedLocalTimeOfDay v' z') =
  compare v v' <> compare z z'
compareSqlValue (SqlZonedLocalTimeOfDay _ _) _ = LT
compareSqlValue _ (SqlZonedLocalTimeOfDay _ _) = GT
compareSqlValue (SqlLocalTime v) (SqlLocalTime v') = compare v v'
compareSqlValue (SqlLocalTime _) _ = LT
compareSqlValue _ (SqlLocalTime _) = GT
compareSqlValue (SqlZonedTime (ZonedTime v z)) (SqlZonedTime (ZonedTime v' z')) =
  compare v v' <> compare z z'
compareSqlValue (SqlZonedTime _) _ = LT
compareSqlValue _ (SqlZonedTime _) = GT
compareSqlValue (SqlUTCTime v) (SqlUTCTime v') = compare v v'
compareSqlValue (SqlUTCTime _) _ = LT
compareSqlValue _ (SqlUTCTime _) = GT
compareSqlValue (SqlDiffTime v) (SqlDiffTime v') = compare v v'
compareSqlValue (SqlDiffTime _) _ = LT
compareSqlValue _ (SqlDiffTime _) = GT
compareSqlValue (SqlPOSIXTime v) (SqlPOSIXTime v') = compare v v'
compareSqlValue (SqlPOSIXTime _) _ = LT
compareSqlValue _ (SqlPOSIXTime _) = GT
compareSqlValue (SqlEpochTime v) (SqlEpochTime v') = compare v v'
compareSqlValue (SqlEpochTime _) _ = LT
compareSqlValue _ (SqlEpochTime _) = GT
compareSqlValue (SqlTimeDiff v) (SqlTimeDiff v') = compare v v'
compareSqlValue (SqlTimeDiff _) _ = LT
compareSqlValue _ (SqlTimeDiff _) = GT
compareSqlValue SqlNull SqlNull = EQ
