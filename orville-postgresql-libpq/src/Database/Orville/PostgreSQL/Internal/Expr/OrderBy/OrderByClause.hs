{-|
Module    : Database.Orville.PostgreSQL.Expr.OrderBy.OrderByClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByClause
  ( OrderByClause
  , orderByClauseToSql
  , orderByClause
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr (OrderByExpr, orderByExprToSql)
import           Database.Orville.PostgreSQL.Internal.RawSql                   (RawSql, fromString)

newtype OrderByClause =
  OrderByClause RawSql

orderByClauseToSql :: OrderByClause -> RawSql
orderByClauseToSql (OrderByClause sql) = sql

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (fromString "ORDER BY " <> orderByExprToSql expr)
