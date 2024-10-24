{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Values
  ( ValuesExpr
  , valuesExpr
  , defaultValuesExpr
  , valuesQueryExpr
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Expr.OrderBy (OrderByExpr)
import Orville.PostgreSQL.Expr.Query (QueryExpr)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, rowValueConstructor)
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
  Construct a 'ValuesExpr' for the given 'ValueExpression's, and any of the supported
  optional clauses.

@since 1.1.0.0
-}
valuesExpr ::
  NonEmpty (NonEmpty ValueExpression) ->
  Maybe OrderByExpr ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  ValuesExpr
valuesExpr vals mbOrderBy mbLimit mbOffset =
  let
    opts =
      RawSql.intercalate RawSql.space $
        catMaybes
          [ fmap RawSql.toRawSql mbOrderBy
          , fmap RawSql.toRawSql mbLimit
          , fmap RawSql.toRawSql mbOffset
          ]
  in
    ValuesExpr $
      RawSql.fromString "VALUES "
        <> RawSql.intercalate RawSql.comma (fmap rowValueConstructor vals)
        <> opts

{- |
  Constructs the SQL @VALUES(DEFAULT)@. Use to insert a default row. Note that this cannot
  be used as a valid query expression, unlike a 'ValuesExpr' constructed using 'valuesExpr'

@since 1.1.0.0
-}
defaultValuesExpr :: ValuesExpr
defaultValuesExpr = ValuesExpr $ RawSql.fromString "VALUES(DEFAULT)"

{- |
  Use a 'ValuesExpr' as a 'QueryExpr'.

@since 1.1.0.0
-}
valuesQueryExpr :: ValuesExpr -> QueryExpr
valuesQueryExpr = RawSql.unsafeFromRawSql . RawSql.toRawSql
