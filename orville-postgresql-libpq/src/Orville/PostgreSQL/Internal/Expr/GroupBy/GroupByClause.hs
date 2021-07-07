{- |
Module    : Orville.PostgreSQL.Expr.GroupBy.GroupByClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByClause
  ( GroupByClause,
    groupByClause,
  )
where

import Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByExpr (GroupByExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype GroupByClause
  = GroupByClause RawSql.RawSql
  deriving RawSql.SqlExpression

groupByClause :: GroupByExpr -> GroupByClause
groupByClause expr = GroupByClause (RawSql.fromString "GROUP BY " <> RawSql.toRawSql expr)
