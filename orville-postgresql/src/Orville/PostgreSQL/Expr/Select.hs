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
  , selectClauseDefault
  , selectClauseDistinct
  )
where

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

{-
    Helper function for constructing SelectClause

@since 1.1.0.0
-}
selectClauseDefault :: SelectClause
selectClauseDefault = selectClause (selectExpr Nothing)

{-
    Helper function for constructing SelectClause with Distinct

@since 1.1.0.0
-}
selectClauseDistinct :: SelectClause
selectClauseDistinct = selectClause (selectExpr $ Just Distinct)
