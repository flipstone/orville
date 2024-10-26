{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Filter
  ( FilterExpr
  , filterExpr
  ) where

import qualified Orville.PostgreSQL.Expr.WhereClause as WhereClause
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | 'FilterExpr' represents the @FILTER@ subexpression E.G.

> FILTER (WHERE some_boolean_expr)


'FilterExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FilterExpr = FilterExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds a 'FilterExpr' with the given 'WhereClause.WhereClause'

@since 1.1.0.0
-}
filterExpr :: WhereClause.WhereClause -> FilterExpr
filterExpr whereClause =
  FilterExpr $
    RawSql.fromString "FILTER"
      <> RawSql.parenthesized (RawSql.toRawSql whereClause)
