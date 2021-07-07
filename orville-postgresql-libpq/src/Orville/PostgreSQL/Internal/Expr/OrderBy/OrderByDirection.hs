{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByDirection
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection
  ( OrderByDirection,
    ascendingOrder,
    descendingOrder,
    orderByDirectionToSql,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByDirection = OrderByDirection RawSql.RawSql

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"

orderByDirectionToSql :: OrderByDirection -> RawSql.RawSql
orderByDirectionToSql (OrderByDirection sql) = sql
