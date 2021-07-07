{- |
Module    : Orville.PostgreSQL.Expr.GroupBy.GroupByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByExpr
  ( GroupByExpr,
    appendGroupBy,
    groupByExpr,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype GroupByExpr = GroupByExpr RawSql.RawSql
  deriving RawSql.SqlExpression

appendGroupBy :: GroupByExpr -> GroupByExpr -> GroupByExpr
appendGroupBy (GroupByExpr a) (GroupByExpr b) =
  GroupByExpr (a <> RawSql.comma <> b)

groupByExpr :: RawSql.RawSql -> GroupByExpr
groupByExpr sql =
  GroupByExpr $ sql
