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
import qualified Database.Orville.PostgreSQL.Internal.RawSql                   as RawSql

newtype OrderByClause =
  OrderByClause RawSql.RawSql

orderByClauseToSql :: OrderByClause -> RawSql.RawSql
orderByClauseToSql (OrderByClause sql) = sql

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> orderByExprToSql expr)
