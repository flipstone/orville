{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Select
  ( SelectClause
  , selectClause
  , SelectExpr
  , selectExpr
  , Distinct (Distinct)
  , DistinctOnExpr
  , selectDistinctOnExpr
  )
where

import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the @SELECT@ part of a SQL query. E.G.

> SELECT

or

> SELECT DISTINCT

'SelectClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.0.0.0
-}
newtype SelectClause
  = SelectClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SelectClause' using the given 'SelectExpr', which may indicate
  that this is a @DISTINCT@ select.

@since 1.0.0.0
-}
selectClause :: SelectExpr -> SelectClause
selectClause expr = SelectClause (RawSql.fromString "SELECT " <> RawSql.toRawSql expr)

{- |
Type to represent any expression modifying the @SELECT@ part of a SQL. E.G.

> DISTINCT

'SelectExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.0.0.0
-}
newtype SelectExpr = SelectExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A simple value type used to indicate that a @SELECT@ should be distinct when
  constructing a 'SelectExpr'.

@since 1.0.0.0
-}
data Distinct = Distinct

{- |
  Constructs a 'SelectExpr' that may or may not make the @SELECT@ distinct,
  depending on whether 'Just Distinct' is passed or not.

@since 1.0.0.0
-}
selectExpr :: Maybe Distinct -> SelectExpr
selectExpr mbDistinct =
  SelectExpr . RawSql.fromString $
    case mbDistinct of
      Just Distinct -> "DISTINCT "
      Nothing -> ""

{- | Type to represent a expression to be used as part of a @DISTINCT@ modifer of a @SELECT@. E.G. the
foo in

> DISTINCT ON (foo)

'SelectExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype DistinctOnExpr = DistinctOnExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Constructs a 'SelectExpr' that may or may not make the @SELECT@ distinct on the given expression.

Note that PostgreSQL states the distinct expressions must match the leftmost order by expressions if
they are both used, but this function does not enforce that.

@since 1.1.0.0
-}
selectDistinctOnExpr :: Maybe (NEL.NonEmpty DistinctOnExpr) -> SelectExpr
selectDistinctOnExpr mbDistinctOns =
  SelectExpr $
    case mbDistinctOns of
      Just ons -> RawSql.fromString "DISTINCT ON " <> RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace ons)
      Nothing -> mempty
