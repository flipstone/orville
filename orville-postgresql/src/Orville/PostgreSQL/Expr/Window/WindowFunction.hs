{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Window.WindowFunction
  ( windowFunction
  , rowNumberWindowFunction
  , rankWindowFunction
  , denseRankWindowFunction
  , percentRankWindowFunction
  , arrayAggWindowFunction
  , avgWindowFunction
  , bitAndWindowFunction
  , bitOrWindowFunction
  , bitXorWindowFunction
  , boolAndWindowFunction
  , boolOrWindowFunction
  , countWindowFunction
  , everyWindowFunction
  , jsonAggWindowFunction
  , jsonObjectAggWindowFunction
  , jsonbAggWindowFunction
  , jsonbObjectAggWindowFunction
  , maxWindowFunction
  , minWindowFunction
  , stringAggWindowFunction
  , sumWindowFunction
  , corWindowFunction
  , covarPopWindowFunction
  , covarSampWindowFunction
  , stddevPopWindowFunction
  , stddevSampWindowFunction
  , varPopWindowFunction
  , varSampWindowFunction
  )
where

import qualified Orville.PostgreSQL.Expr.Filter as Filter
import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import qualified Orville.PostgreSQL.Expr.Name as Name
import qualified Orville.PostgreSQL.Expr.ValueExpression as ValueExpression
import Orville.PostgreSQL.Expr.Window.WindowDefinitionExpr (WindowDefinitionExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
The @row_number@ window function

@since 1.1.0.0
-}
rowNumberWindowFunction :: Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
rowNumberWindowFunction = windowFunction (functionName "row_number") mempty

{- |
The @rank@ window function

@since 1.1.0.0
-}
rankWindowFunction :: Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
rankWindowFunction = windowFunction (functionName "rank") mempty

{- |
The @dense_rank@ window function

@since 1.1.0.0
-}
denseRankWindowFunction :: Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
denseRankWindowFunction = windowFunction (functionName "dense_rank") mempty

{- |
The @percent_rank@ window function

@since 1.1.0.0
-}
percentRankWindowFunction :: Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
percentRankWindowFunction = windowFunction (functionName "percent_rank") mempty

{- | Build a 'ValueExpression' that represents a window function call. It is up to the caller to
ensure this makes sense as windowing functions are not allowed in all places.

@since 1.1.0.0
-}
windowFunction ::
  FunctionName ->
  [ValueExpression.ValueExpression] ->
  Maybe Filter.FilterExpr ->
  WindowDefinitionExpr ->
  ValueExpression.ValueExpression
windowFunction function parameters mbFilter windowDef =
  let
    filtering =
      case mbFilter of
        Nothing -> mempty
        Just filterExpr -> RawSql.space <> RawSql.toRawSql filterExpr
  in
    RawSql.unsafeFromRawSql $
      RawSql.toRawSql (ValueExpression.functionCall function parameters)
        <> filtering
        <> RawSql.space
        <> RawSql.fromString "OVER"
        <> RawSql.parenthesized (RawSql.toRawSql windowDef)

{- | The SQL @array_agg@ window function.

@since 1.1.0.0
-}
arrayAggWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
arrayAggWindowFunction = singleParameterWindowFunction Name.arrayAggFunctionName

{- | The SQL @avg@ window function.

@since 1.1.0.0
-}
avgWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
avgWindowFunction = singleParameterWindowFunction Name.avgFunctionName

{- | The SQL @bit_and@ window function.

@since 1.1.0.0
-}
bitAndWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
bitAndWindowFunction = singleParameterWindowFunction Name.bitAndFunctionName

{- | The SQL @bit_or@ window function.

@since 1.1.0.0
-}
bitOrWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
bitOrWindowFunction = singleParameterWindowFunction Name.bitOrFunctionName

{- | The SQL @bit_xor@ window function.

@since 1.1.0.0
-}
bitXorWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
bitXorWindowFunction = singleParameterWindowFunction Name.bitXorFunctionName

{- | The SQL @bool_and@ window function.

@since 1.1.0.0
-}
boolAndWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
boolAndWindowFunction = singleParameterWindowFunction Name.boolAndFunctionName

{- | The SQL @bool_or@ window function.

@since 1.1.0.0
-}
boolOrWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
boolOrWindowFunction = singleParameterWindowFunction Name.boolOrFunctionName

{- | The SQL @count@ window function.

@since 1.1.0.0
-}
countWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
countWindowFunction = singleParameterWindowFunction Name.countFunctionName

{- | The SQL @every@ window function.

@since 1.1.0.0
-}
everyWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
everyWindowFunction = singleParameterWindowFunction Name.everyFunctionName

{- | The SQL @json_agg@ window function.

@since 1.1.0.0
-}
jsonAggWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
jsonAggWindowFunction = singleParameterWindowFunction Name.jsonAggFunctionName

{- | The SQL @json_object_agg@ window function.

@since 1.1.0.0
-}
jsonObjectAggWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
jsonObjectAggWindowFunction param1 param2 =
  windowFunction Name.jsonObjectAggFunctionName [param1, param2]

{- | The SQL @jsonb_agg@ window function.

@since 1.1.0.0
-}
jsonbAggWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
jsonbAggWindowFunction = singleParameterWindowFunction Name.jsonbAggFunctionName

{- | The SQL @jsonb_object_agg@ window function.

@since 1.1.0.0
-}
jsonbObjectAggWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
jsonbObjectAggWindowFunction param1 param2 =
  windowFunction Name.jsonbObjectAggFunctionName [param1, param2]

{- | The SQL @max@ window function.

@since 1.1.0.0
-}
maxWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
maxWindowFunction = singleParameterWindowFunction Name.maxFunctionName

{- | The SQL @min@ window function.

@since 1.1.0.0
-}
minWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
minWindowFunction = singleParameterWindowFunction Name.minFunctionName

{- | The SQL @string_agg@ window function.

@since 1.1.0.0
-}
stringAggWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
stringAggWindowFunction param1 param2 =
  windowFunction Name.stringAggFunctionName [param1, param2]

{- | The SQL @sum@ window function.

@since 1.1.0.0
-}
sumWindowFunction ::
  ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
sumWindowFunction = singleParameterWindowFunction Name.sumFunctionName

{- | The SQL @cor@ window function.

@since 1.1.0.0
-}
corWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
corWindowFunction param1 param2 =
  windowFunction Name.corFunctionName [param1, param2]

{- | The SQL @covar_pop@ window function.

@since 1.1.0.0
-}
covarPopWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
covarPopWindowFunction param1 param2 =
  windowFunction Name.covarPopFunctionName [param1, param2]

{- | The SQL @covar_samp@ window function.

@since 1.1.0.0
-}
covarSampWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
covarSampWindowFunction param1 param2 =
  windowFunction Name.covarSampFunctionName [param1, param2]

{- | The SQL @stddev_pop@ window function.

@since 1.1.0.0
-}
stddevPopWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
stddevPopWindowFunction param1 param2 =
  windowFunction Name.stddevPopFunctionName [param1, param2]

{- | The SQL @stddev_samp@ window function.

@since 1.1.0.0
-}
stddevSampWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
stddevSampWindowFunction param1 param2 =
  windowFunction Name.stddevSampFunctionName [param1, param2]

{- | The SQL @var_pop@ window function.

@since 1.1.0.0
-}
varPopWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
varPopWindowFunction param1 param2 =
  windowFunction Name.varPopFunctionName [param1, param2]

{- | The SQL @var_samp@ window function.

@since 1.1.0.0
-}
varSampWindowFunction ::
  ValueExpression.ValueExpression -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
varSampWindowFunction param1 param2 =
  windowFunction Name.varSampFunctionName [param1, param2]

singleParameterWindowFunction ::
  FunctionName -> ValueExpression.ValueExpression -> Maybe Filter.FilterExpr -> WindowDefinitionExpr -> ValueExpression.ValueExpression
singleParameterWindowFunction function parameter =
  windowFunction function (pure parameter)
