{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Aggregate
  ( AggregateOptionExpr
  , aggregateExpression
  , aggregateStarExpression
  , aggregateWithinGroupExpression
  , arrayAggAggregateFunction
  , avgAggregateFunction
  , bitAndAggregateFunction
  , bitOrAggregateFunction
  , bitXorAggregateFunction
  , boolAndAggregateFunction
  , boolOrAggregateFunction
  , countExprAggregateFunction
  , count1AggregateFunction
  , countColumnAggregateFunction
  , countAggregateFunction
  , everyAggregateFunction
  , jsonAggAggregateFunction
  , jsonObjectAggAggregateFunction
  , jsonbAggAggregateFunction
  , jsonbObjectAggAggregateFunction
  , maxAggregateFunction
  , minAggregateFunction
  , stringAggAggregateFunction
  , sumAggregateFunction
  , corAggregateFunction
  , covarPopAggregateFunction
  , covarSampAggregateFunction
  , stddevPopAggregateFunction
  , stddevSampAggregateFunction
  , varPopAggregateFunction
  , varSampAggregateFunction
  )
where

import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Expr.Filter as Filter
import Orville.PostgreSQL.Expr.Name (FunctionName)
import qualified Orville.PostgreSQL.Expr.Name as Name
import qualified Orville.PostgreSQL.Expr.OrderBy as OrderBy
import qualified Orville.PostgreSQL.Expr.ValueExpression as ValueExpression
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | 'AggregateOptionExpr' represents the options to aggregate functions. E.G.

> ALL

or

> DISTINCT

'AggregateOptionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype AggregateOptionExpr = AggregateOptionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds an aggregate expression. Note that it is up to the caller to ensure the validity of all of the arguments and that the resulting 'ValueExpression.ValueExpression' is used in an appropriate manner.

@since 1.1.0.0
-}
aggregateExpression :: FunctionName -> Maybe AggregateOptionExpr -> NEL.NonEmpty ValueExpression.ValueExpression -> Maybe OrderBy.OrderByClause -> Maybe Filter.FilterExpr -> ValueExpression.ValueExpression
aggregateExpression function mbAggOption parameters mbOrderByClause mbFilter =
  let
    fnCall =
      RawSql.toRawSql function
        <> RawSql.parenthesized
          ( ( case mbAggOption of
                Nothing -> mempty
                Just aggOption ->
                  RawSql.toRawSql aggOption
                    <> RawSql.space
            )
              <> RawSql.intercalate RawSql.comma parameters
              <> case mbOrderByClause of
                Nothing -> mempty
                Just orderByClause -> RawSql.toRawSql orderByClause
          )

    filtering =
      case mbFilter of
        Nothing -> mempty
        Just filterExpr -> RawSql.space <> RawSql.toRawSql filterExpr
  in
    RawSql.unsafeFromRawSql $
      fnCall
        <> filtering

{- | Builds an aggregate expression with a * as the function argument. Note that it is up to the caller to ensure the validity of all of the arguments and that the resulting 'ValueExpression.ValueExpression' is used in an appropriate manner.

@since 1.1.0.0
-}
aggregateStarExpression :: FunctionName -> Maybe Filter.FilterExpr -> ValueExpression.ValueExpression
aggregateStarExpression function mbFilter =
  let
    parameters = pure . RawSql.unsafeFromRawSql $ RawSql.fromString "*"
    filtering =
      case mbFilter of
        Nothing -> mempty
        Just filterExpr -> RawSql.space <> RawSql.toRawSql filterExpr
  in
    RawSql.unsafeFromRawSql $
      RawSql.toRawSql (ValueExpression.functionCall function parameters)
        <> filtering

{- | Builds an orderd-set aggregate expression. These have the 'OrderBy.OrderByClause' as an argument to the aggregating function. Note that it is up to the caller to ensure the validity of all of the arguments and that the resulting 'ValueExpression.ValueExpression' is used in an appropriate manner.

@since 1.1.0.0
-}
aggregateWithinGroupExpression :: FunctionName -> [ValueExpression.ValueExpression] -> OrderBy.OrderByClause -> Maybe Filter.FilterExpr -> ValueExpression.ValueExpression
aggregateWithinGroupExpression function parameters orderByClause mbFilter =
  let
    within =
      RawSql.fromString " WITHIN GROUP "
        <> RawSql.parenthesized (RawSql.toRawSql orderByClause)
    filtering =
      case mbFilter of
        Nothing -> mempty
        Just filterExpr -> RawSql.space <> RawSql.toRawSql filterExpr
  in
    RawSql.unsafeFromRawSql $
      RawSql.toRawSql (ValueExpression.functionCall function parameters)
        <> within
        <> filtering

{- | The SQL @array_agg@ aggregate function.

@since 1.1.0.0
-}
arrayAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
arrayAggAggregateFunction =
  singleParameterAggregateFunction Name.arrayAggFunctionName

{- | The SQL @avg@ aggregate function.

@since 1.1.0.0
-}
avgAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
avgAggregateFunction =
  singleParameterAggregateFunction Name.avgFunctionName

{- | The SQL @bit_and@ aggregate function.

@since 1.1.0.0
-}
bitAndAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
bitAndAggregateFunction =
  singleParameterAggregateFunction Name.bitAndFunctionName

{- | The SQL @bit_or@ aggregate function.

@since 1.1.0.0
-}
bitOrAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
bitOrAggregateFunction =
  singleParameterAggregateFunction Name.bitOrFunctionName

{- | The SQL @bit_xor@ aggregate function.

@since 1.1.0.0
-}
bitXorAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
bitXorAggregateFunction =
  singleParameterAggregateFunction Name.bitXorFunctionName

{- | The SQL @bool_and@ aggregate function.

@since 1.1.0.0
-}
boolAndAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
boolAndAggregateFunction =
  singleParameterAggregateFunction Name.boolAndFunctionName

{- | The SQL @bool_or@ aggregate function.

@since 1.1.0.0
-}
boolOrAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
boolOrAggregateFunction =
  singleParameterAggregateFunction Name.boolOrFunctionName

{- | A simplified version of 'countAggregateFunction' for the SQL @count@ of a
   'ValueExpression.ValueExpression'

@since 1.1.0.0
-}
countExprAggregateFunction ::
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression
countExprAggregateFunction valExpr =
  singleParameterAggregateFunction Name.countFunctionName Nothing valExpr Nothing Nothing

{- | A simplified version of 'countAggregateFunction' for the SQL @count(1)@

@since 1.1.0.0
-}
count1AggregateFunction ::
  ValueExpression.ValueExpression
count1AggregateFunction =
  countExprAggregateFunction (RawSql.unsafeFromRawSql . RawSql.intDecLiteral $ 1)

{- | A simplified version of 'countAggregateFunction' for the SQL @count@ of a column

@since 1.1.0.0
-}
countColumnAggregateFunction ::
  Name.QualifiedOrUnqualified Name.ColumnName ->
  ValueExpression.ValueExpression
countColumnAggregateFunction =
  countExprAggregateFunction . ValueExpression.columnReference

{- | The SQL @count@ aggregate function.

@since 1.1.0.0
-}
countAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
countAggregateFunction =
  singleParameterAggregateFunction Name.countFunctionName

{- | The SQL @every@ aggregate function.

@since 1.1.0.0
-}
everyAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
everyAggregateFunction =
  singleParameterAggregateFunction Name.everyFunctionName

{- | The SQL @json_agg@ aggregate function.

@since 1.1.0.0
-}
jsonAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
jsonAggAggregateFunction =
  singleParameterAggregateFunction Name.jsonAggFunctionName

{- | The SQL @json_object_agg@ aggregate function.

@since 1.1.0.0
-}
jsonObjectAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
jsonObjectAggAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.jsonObjectAggFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @jsonb_agg@ aggregate function.

@since 1.1.0.0
-}
jsonbAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
jsonbAggAggregateFunction =
  singleParameterAggregateFunction Name.jsonbAggFunctionName

{- | The SQL @jsonb_object_agg@ aggregate function.

@since 1.1.0.0
-}
jsonbObjectAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
jsonbObjectAggAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.jsonbObjectAggFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @max@ aggregate function.

@since 1.1.0.0
-}
maxAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
maxAggregateFunction =
  singleParameterAggregateFunction Name.maxFunctionName

{- | The SQL @min@ aggregate function.

@since 1.1.0.0
-}
minAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
minAggregateFunction =
  singleParameterAggregateFunction Name.minFunctionName

{- | The SQL @string_agg@ aggregate function.

@since 1.1.0.0
-}
stringAggAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
stringAggAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.stringAggFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @sum@ aggregate function.

@since 1.1.0.0
-}
sumAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
sumAggregateFunction =
  singleParameterAggregateFunction Name.sumFunctionName

{- | The SQL @cor@ aggregate function.

@since 1.1.0.0
-}
corAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
corAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.corFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @covar_pop@ aggregate function.

@since 1.1.0.0
-}
covarPopAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
covarPopAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.covarPopFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @covar_samp@ aggregate function.

@since 1.1.0.0
-}
covarSampAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
covarSampAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.covarSampFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @stddev_pop@ aggregate function.

@since 1.1.0.0
-}
stddevPopAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
stddevPopAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.stddevPopFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @stddev_samp@ aggregate function.

@since 1.1.0.0
-}
stddevSampAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
stddevSampAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.stddevSampFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @var_pop@ aggregate function.

@since 1.1.0.0
-}
varPopAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
varPopAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.varPopFunctionName mbAggOption (param1 NEL.:| (pure param2))

{- | The SQL @var_samp@ aggregate function.

@since 1.1.0.0
-}
varSampAggregateFunction ::
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
varSampAggregateFunction mbAggOption param1 param2 =
  aggregateExpression Name.varSampFunctionName mbAggOption (param1 NEL.:| (pure param2))

-- internal helper to make building up the aggregates that only take a single parameter.
singleParameterAggregateFunction ::
  FunctionName ->
  Maybe AggregateOptionExpr ->
  ValueExpression.ValueExpression ->
  Maybe OrderBy.OrderByClause ->
  Maybe Filter.FilterExpr ->
  ValueExpression.ValueExpression
singleParameterAggregateFunction function mbAggOption parameter =
  aggregateExpression function mbAggOption (pure parameter)
