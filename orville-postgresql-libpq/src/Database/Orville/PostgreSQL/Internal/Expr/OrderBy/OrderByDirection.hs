{-|
Module    : Database.Orville.PostgreSQL.Expr.OrderBy.OrderByDirection
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection
  ( OrderByDirection
  , ascendingOrder
  , descendingOrder
  , orderByDirectionToSql
  ) where

import Database.Orville.PostgreSQL.Internal.RawSql (RawSql, fromString)

newtype OrderByDirection = OrderByDirection RawSql

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ fromString "DESC"

orderByDirectionToSql :: OrderByDirection -> RawSql
orderByDirectionToSql (OrderByDirection sql) = sql
