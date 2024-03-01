{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.GroupBy
  ( GroupByClause
  , groupByClause
  , GroupByExpr
  , appendGroupByExpr
  , groupByColumnsExpr
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL group by clause. E.G.

> GROUP BY team_name

'GroupByClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype GroupByClause
  = GroupByClause RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create a full SQL GROUP BY clause with the given expression.

@since 1.0.0.0
-}
groupByClause :: GroupByExpr -> GroupByClause
groupByClause expr = GroupByClause (RawSql.fromString "GROUP BY " <> RawSql.toRawSql expr)

{- |
Type to represent a SQL group by expression (the part that follows the
@GROUP BY@ in SQL). E.G.

> team_name

'GroupByExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype GroupByExpr
  = GroupByExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
@since 1.0.0.0
-}
instance Semigroup GroupByExpr where
  (<>) = appendGroupByExpr

{- | Combines two 'GroupByExpr's with a comma between them.

@since 1.0.0.0
-}
appendGroupByExpr :: GroupByExpr -> GroupByExpr -> GroupByExpr
appendGroupByExpr (GroupByExpr a) (GroupByExpr b) =
  GroupByExpr (a <> RawSql.commaSpace <> b)

{- | Create a 'GroupByExpr' from the given 'ColumnName's.

@since 1.0.0.0
-}
groupByColumnsExpr :: NonEmpty (Qualified ColumnName) -> GroupByExpr
groupByColumnsExpr =
  GroupByExpr . RawSql.intercalate RawSql.commaSpace
