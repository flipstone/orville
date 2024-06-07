{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Window.WindowFunction
  ( rowNumber
  , rank
  , denseRank
  , percentRank
  , WindowFunctionExpr
  , windowFunction
  )
where

import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import qualified Orville.PostgreSQL.Expr.ValueExpression as ValueExpression
import qualified Orville.PostgreSQL.Expr.WhereClause as WhereClause
import Orville.PostgreSQL.Expr.Window.WindowDefinitionExpr (WindowDefinitionExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
The @row_number@ window function

@since 1.1.0.0
-}
rowNumber :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> WindowFunctionExpr
rowNumber = windowFunction (functionName "row_number") []

{- |
The @rank@ window function

@since 1.1.0.0
-}
rank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> WindowFunctionExpr
rank = windowFunction (functionName "rank") []

{- |
The @dense_rank@ window function

@since 1.1.0.0
-}
denseRank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> WindowFunctionExpr
denseRank = windowFunction (functionName "dense_rank") []

{- |
The @percent_rank@ window function

@since 1.1.0.0
-}
percentRank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> WindowFunctionExpr
percentRank = windowFunction (functionName "percent_rank") []

{- |
Type to represent a SQL window expression (the part that follows the @WINDOW@ in SQL). E.G.

> foo, bar

'WindowFunctionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype WindowFunctionExpr = WindowFunctionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- |
Build a 'WindowFunctionExpr'.

@since 1.1.0.0
-}
windowFunction :: FunctionName -> [ValueExpression.ValueExpression] -> Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> WindowFunctionExpr
windowFunction function parameters mbWhereClause windowDef =
  let
    filtering =
      case mbWhereClause of
        Nothing -> mempty
        Just whereClause ->
          RawSql.fromString "FILTER"
            <> RawSql.parenthesized (RawSql.toRawSql whereClause)
  in
    WindowFunctionExpr $
      RawSql.toRawSql (ValueExpression.functionCall function parameters)
        <> RawSql.space
        <> filtering
        <> RawSql.space
        <> RawSql.fromString "OVER"
        <> RawSql.space
        <> RawSql.toRawSql windowDef
