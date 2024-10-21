{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.FunctionName
  ( FunctionName
  , functionName
  , arrayAggFunctionName
  , avgFunctionName
  , bitAndFunctionName
  , bitOrFunctionName
  , bitXorFunctionName
  , boolAndFunctionName
  , boolOrFunctionName
  , everyFunctionName
  , jsonAggFunctionName
  , jsonObjectAggFunctionName
  , jsonbAggFunctionName
  , jsonbObjectAggFunctionName
  , maxFunctionName
  , minFunctionName
  , stringAggFunctionName
  , sumFunctionName
  , corFunctionName
  , covarPopFunctionName
  , covarSampFunctionName
  , stddevPopFunctionName
  , stddevSampFunctionName
  , varPopFunctionName
  , varSampFunctionName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL function name. 'FunctionName' values constructed
via the 'functionName' function will be properly escaped as part of the
generated SQL. E.G.

> "some_function_name"

'FunctionName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype FunctionName
  = FunctionName Identifier
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    , -- | @since 1.0.0.0
      IdentifierExpression
    )

{- |
Construct a 'FunctionName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
functionName :: String -> FunctionName
functionName =
  FunctionName . identifier

{- |
The @array_agg@ window function name

@since 1.1.0.0
-}
arrayAggFunctionName :: FunctionName
arrayAggFunctionName = functionName "array_agg"

{- |
The @avg@ window function name

@since 1.1.0.0
-}
avgFunctionName :: FunctionName
avgFunctionName = functionName "avg"

{- |
The @bit_and@ window function name

@since 1.1.0.0
-}
bitAndFunctionName :: FunctionName
bitAndFunctionName = functionName "bit_and"

{- |
The @bit_or@ window function name

@since 1.1.0.0
-}
bitOrFunctionName :: FunctionName
bitOrFunctionName = functionName "bit_or"

{- |
The @bit_xor@ window function name

@since 1.1.0.0
-}
bitXorFunctionName :: FunctionName
bitXorFunctionName = functionName "bit_xor"

{- |
The @bool_and@ window function name

@since 1.1.0.0
-}
boolAndFunctionName :: FunctionName
boolAndFunctionName = functionName "bool_and"

{- |
The @bool_or@ window function name

@since 1.1.0.0
-}
boolOrFunctionName :: FunctionName
boolOrFunctionName = functionName "bool_or"

{- |
The @every@ window function name

@since 1.1.0.0
-}
everyFunctionName :: FunctionName
everyFunctionName = functionName "every"

{- |
The @json_agg@ window function name

@since 1.1.0.0
-}
jsonAggFunctionName :: FunctionName
jsonAggFunctionName = functionName "json_agg"

{- |
The @json_object_agg@ window function name

@since 1.1.0.0
-}
jsonObjectAggFunctionName :: FunctionName
jsonObjectAggFunctionName = functionName "json_object_agg"

{- |
The @jsonb_agg@ window function name

@since 1.1.0.0
-}
jsonbAggFunctionName :: FunctionName
jsonbAggFunctionName = functionName "jsonb_agg"

{- |
The @jsonb_object_agg@ window function name

@since 1.1.0.0
-}
jsonbObjectAggFunctionName :: FunctionName
jsonbObjectAggFunctionName = functionName "jsonb_object_agg"

{- |
The @max@ window function name

@since 1.1.0.0
-}
maxFunctionName :: FunctionName
maxFunctionName = functionName "max"

{- |
The @min@ window function name

@since 1.1.0.0
-}
minFunctionName :: FunctionName
minFunctionName = functionName "min"

{- |
The @string_agg@ window function name

@since 1.1.0.0
-}
stringAggFunctionName :: FunctionName
stringAggFunctionName = functionName "string_agg"

{- |
The @sum@ window function name

@since 1.1.0.0
-}
sumFunctionName :: FunctionName
sumFunctionName = functionName "sum"

{- |
The @cor@ window function name

@since 1.1.0.0
-}
corFunctionName :: FunctionName
corFunctionName = functionName "cor"

{- |
The @covar_pop@ window function name

@since 1.1.0.0
-}
covarPopFunctionName :: FunctionName
covarPopFunctionName = functionName "covar_pop"

{- |
The @covar_samp@ window function name

@since 1.1.0.0
-}
covarSampFunctionName :: FunctionName
covarSampFunctionName = functionName "covar_samp"

{- |
The @stddev_pop@ window function name

@since 1.1.0.0
-}
stddevPopFunctionName :: FunctionName
stddevPopFunctionName = functionName "stddev_pop"

{- |
The @stddev_samp@ window function name

@since 1.1.0.0
-}
stddevSampFunctionName :: FunctionName
stddevSampFunctionName = functionName "stddev_samp"

{- |
The @var_pop@ window function name

@since 1.1.0.0
-}
varPopFunctionName :: FunctionName
varPopFunctionName = functionName "var_pop"

{- |
The @var_samp@ window function name

@since 1.1.0.0
-}
varSampFunctionName :: FunctionName
varSampFunctionName = functionName "var_samp"
