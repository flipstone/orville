{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
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
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL group by clause.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a group by clause by hand and use it in a
place that expected a 'GroupByClause', that could be done as

 > RawSql.unsafeSqlExpression "GROUP BY <my unusual group by>"

@since 0.10.0.0
-}
newtype GroupByClause
  = GroupByClause RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

groupByClause :: GroupByExpr -> GroupByClause
groupByClause expr = GroupByClause (RawSql.fromString "GROUP BY " <> RawSql.toRawSql expr)

{- |
Type to represent a SQL group by expression (the part that follows the @GROUP
BY@ in sql).

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a group by expression by hand and use it in a
place that expected a 'GroupByExpr, that could be done as

 > RawSql.unsafeSqlExpression "<my unusual group by>"

@since 0.10.0.0
-}
newtype GroupByExpr
  = GroupByExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

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
