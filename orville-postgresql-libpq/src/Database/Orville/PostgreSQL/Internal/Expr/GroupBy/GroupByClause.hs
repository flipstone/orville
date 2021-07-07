{- |
Module    : Database.Orville.PostgreSQL.Expr.GroupBy.GroupByClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByClause
  ( GroupByClause,
    groupByClauseToSql,
    groupByClause,
  )
where

import Database.Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByExpr (GroupByExpr, groupByExprToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

newtype GroupByClause
  = GroupByClause RawSql.RawSql

groupByClauseToSql :: GroupByClause -> RawSql.RawSql
groupByClauseToSql (GroupByClause sql) = sql

groupByClause :: GroupByExpr -> GroupByClause
groupByClause expr = GroupByClause (RawSql.fromString "GROUP BY " <> groupByExprToSql expr)
