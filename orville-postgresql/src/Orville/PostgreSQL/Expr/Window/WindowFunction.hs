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
rowNumber :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> ValueExpression.ValueExpression
rowNumber = windowFunction (functionName "row_number") []

{- |
The @rank@ window function

@since 1.1.0.0
-}
rank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> ValueExpression.ValueExpression
rank = windowFunction (functionName "rank") []

{- |
The @dense_rank@ window function

@since 1.1.0.0
-}
denseRank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> ValueExpression.ValueExpression
denseRank = windowFunction (functionName "dense_rank") []

{- |
The @percent_rank@ window function

@since 1.1.0.0
-}
percentRank :: Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> ValueExpression.ValueExpression
percentRank = windowFunction (functionName "percent_rank") []

{- | Build a 'ValueExpression' that represents a window function call. It is up to the caller to
ensure this makes sense as windowing functions are not allowed in all places.

@since 1.1.0.0
-}
windowFunction :: FunctionName -> [ValueExpression.ValueExpression] -> Maybe WhereClause.WhereClause -> WindowDefinitionExpr -> ValueExpression.ValueExpression
windowFunction function parameters mbWhereClause windowDef =
  let
    filtering =
      case mbWhereClause of
        Nothing -> mempty
        Just whereClause ->
          RawSql.fromString "FILTER"
            <> RawSql.parenthesized (RawSql.toRawSql whereClause)
  in
    RawSql.unsafeFromRawSql $
      RawSql.toRawSql (ValueExpression.functionCall function parameters)
        <> RawSql.space
        <> filtering
        <> RawSql.space
        <> RawSql.fromString "OVER"
        <> RawSql.space
        <> RawSql.toRawSql windowDef
