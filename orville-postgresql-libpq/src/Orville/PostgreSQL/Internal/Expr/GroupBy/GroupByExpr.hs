{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.GroupBy.GroupByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.GroupBy.GroupByExpr
  ( GroupByExpr,
    appendGroupByExpr,
    groupByExpr,
    groupByColumnsExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype GroupByExpr = GroupByExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

appendGroupByExpr :: GroupByExpr -> GroupByExpr -> GroupByExpr
appendGroupByExpr (GroupByExpr a) (GroupByExpr b) =
  GroupByExpr (a <> RawSql.commaSpace <> b)

groupByExpr :: RawSql.RawSql -> GroupByExpr
groupByExpr sql =
  GroupByExpr $ sql

groupByColumnsExpr :: NonEmpty ColumnName -> GroupByExpr
groupByColumnsExpr =
  GroupByExpr . RawSql.intercalate RawSql.commaSpace
