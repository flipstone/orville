{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr
  ( OrderByExpr,
    appendOrderBy,
    orderByExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection (OrderByDirection)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByExpr = OrderByExpr RawSql.RawSql
  deriving RawSql.SqlExpression

appendOrderBy :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderBy (OrderByExpr a) (OrderByExpr b) =
  OrderByExpr (a <> RawSql.comma <> b)

orderByExpr :: RawSql.RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql orderSql =
  OrderByExpr $ sql <> RawSql.space <> RawSql.toRawSql orderSql
