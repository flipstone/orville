{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Values
  ( ValuesExpr
  , valuesExpr
  , valuesExprFromValueExpressions
  , ValuesExprRow
  , valuesExprRow
  , ValuesExprValue
  , valuesExprValue
  , valuesExprDefaultValue
  , valuesQueryExpr
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.FetchClause (FetchClause)
import Orville.PostgreSQL.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Expr.OrderBy (OrderByExpr)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a @VALUES@ statement, e.g.

> VALUES ('Bob',32),('Cindy',33)

@since 1.1.0.0
-}
newtype ValuesExpr = ValuesExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
  Construct a 'ValuesExpr' for the given 'ValuesExprRow's, and any of the supported
  optional clauses.

@since 1.1.0.0
-}
valuesExpr ::
  NonEmpty ValuesExprRow ->
  Maybe OrderByExpr ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  Maybe FetchClause ->
  ValuesExpr
valuesExpr vals mbOrderBy mbLimit mbOffset mbFetch =
  let
    opts =
      RawSql.intercalate RawSql.space $
        catMaybes
          [ fmap RawSql.toRawSql mbOrderBy
          , fmap RawSql.toRawSql mbLimit
          , fmap RawSql.toRawSql mbOffset
          , fmap RawSql.toRawSql mbFetch
          ]
  in
    ValuesExpr $
      RawSql.fromString "VALUES "
        <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql vals)
        <> opts

{- |
  A helper function to construct a 'ValuesExpr' from a non-empty lists of 'ValueExpression's,
  useful for conveinently constructing a 'ValuesExpr' in contexts where you don't need to use
  @DEFAULT@ values or additional clauses.

@since 1.1.0.0
-}
valuesExprFromValueExpressions :: NonEmpty (NonEmpty ValueExpression) -> ValuesExpr
valuesExprFromValueExpressions valExprs =
  let
    rows =
      fmap (valuesExprRow . fmap valuesExprValue) valExprs
  in
    valuesExpr rows Nothing Nothing Nothing Nothing

{- |
  A non-emtpy row of values or @DEFAULT@s used to construct the rows for a 'ValueExpr'.

@since 1.1.0.0
-}
newtype ValuesExprRow = ValuesExprRow RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
  Construct a 'ValuesExprRow' from a non-empty list of 'ValuesExprValue's.

@since 1.1.0.0
-}
valuesExprRow :: NonEmpty ValuesExprValue -> ValuesExprRow
valuesExprRow vals =
  ValuesExprRow
    . RawSql.parenthesized
    $ RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql vals)

{- |
  A value used to construct a 'ValuesExprRow'.

@since 1.1.0.0
-}
newtype ValuesExprValue = ValuesExprValue RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
  Construct a 'ValuesExprValue' from a 'ValueExpression'.

@since 1.1.0.0
-}
valuesExprValue :: ValueExpression -> ValuesExprValue
valuesExprValue = ValuesExprValue . RawSql.toRawSql

{- |
  Construct a @DEFAULT@ 'ValuesExprValue'. Only valid in the context of a 'ValuesExpr' used for an
  @INSERT@ statement, where it indicates that the default value should be used for a column.

@since 1.1.0.0
-}
valuesExprDefaultValue :: ValuesExprValue
valuesExprDefaultValue = ValuesExprValue $ RawSql.fromString "DEFAULT"

{- |
  Use a 'ValuesExpr' as a 'QueryExpr'.

@since 1.1.0.0
-}
valuesQueryExpr :: ValuesExpr -> QueryExpr
valuesQueryExpr = RawSql.unsafeFromRawSql . RawSql.toRawSql
