{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByClause
  ( OrderByClause,
    orderByClause,
  )
where

import Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr (OrderByExpr, orderByExprToSql)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByClause
  = OrderByClause RawSql.RawSql
  deriving (RawSql.ToRawSql)

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> orderByExprToSql expr)
