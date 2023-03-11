{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.GroupBy
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.GroupBy
  ( GroupByClause,
    groupByClause,
    GroupByExpr,
    appendGroupByExpr,
    groupByExpr,
    groupByColumnsExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype GroupByClause
  = GroupByClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

groupByClause :: GroupByExpr -> GroupByClause
groupByClause expr = GroupByClause (RawSql.fromString "GROUP BY " <> RawSql.toRawSql expr)

newtype GroupByExpr = GroupByExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

instance Semigroup GroupByExpr where
  (<>) = appendGroupByExpr

appendGroupByExpr :: GroupByExpr -> GroupByExpr -> GroupByExpr
appendGroupByExpr (GroupByExpr a) (GroupByExpr b) =
  GroupByExpr (a <> RawSql.commaSpace <> b)

groupByExpr :: RawSql.RawSql -> GroupByExpr
groupByExpr sql =
  GroupByExpr $ sql

groupByColumnsExpr :: NonEmpty ColumnName -> GroupByExpr
groupByColumnsExpr =
  GroupByExpr . RawSql.intercalate RawSql.commaSpace
