{- |
Module    : Database.Orville.PostgreSQL.Expr.OrderBy.OrderByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr
  ( OrderByExpr,
    orderByExprToSql,
    appendOrderBy,
    orderByExpr,
  )
where

import Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection (OrderByDirection, orderByDirectionToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByExpr = OrderByExpr RawSql.RawSql

orderByExprToSql :: OrderByExpr -> RawSql.RawSql
orderByExprToSql (OrderByExpr sql) = sql

appendOrderBy :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderBy (OrderByExpr a) (OrderByExpr b) =
  OrderByExpr (a <> RawSql.comma <> b)

orderByExpr :: RawSql.RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql orderSql =
  OrderByExpr $ sql <> RawSql.space <> orderByDirectionToSql orderSql
