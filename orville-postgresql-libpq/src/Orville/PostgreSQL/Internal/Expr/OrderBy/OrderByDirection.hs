{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByDirection
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection
  ( OrderByDirection,
    NullsOrder (..),
    ascendingOrder,
    descendingOrder,
    ascendingOrderWith,
    descendingOrderWith,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByDirection = OrderByDirection RawSql.RawSql
  deriving (RawSql.SqlExpression)

data NullsOrder
  = NullsFirst
  | NullsLast
  deriving (Eq, Show, Ord, Enum, Bounded)

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"

ascendingOrderWith :: NullsOrder -> OrderByDirection
ascendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql ascendingOrder <> RawSql.space <> nullsOrder no

descendingOrderWith :: NullsOrder -> OrderByDirection
descendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql descendingOrder <> RawSql.space <> nullsOrder no

nullsOrder :: NullsOrder -> RawSql.RawSql
nullsOrder no = case no of
  NullsFirst -> RawSql.fromString "NULLS FIRST"
  NullsLast -> RawSql.fromString "NULLS LAST"
