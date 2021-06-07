{-|
Module    : Database.Orville.PostgreSQL.Expr.OrderBy.OrderByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr
  ( OrderByExpr
  , orderByExprToSql
  , appendOrderBy
  , orderByExpr
  ) where

import Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection (OrderByDirection, orderByDirectionToSql)
import Database.Orville.PostgreSQL.Internal.RawSql                        (RawSql, fromString)

newtype OrderByExpr = OrderByExpr RawSql

orderByExprToSql :: OrderByExpr -> RawSql
orderByExprToSql (OrderByExpr sql) = sql

appendOrderBy :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderBy (OrderByExpr a) (OrderByExpr b) =
  OrderByExpr (a <> fromString ", " <> b)

orderByExpr :: RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql orderSql =
  OrderByExpr $ sql <> fromString " " <> orderByDirectionToSql orderSql
