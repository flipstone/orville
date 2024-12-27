{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024-2025
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.TextSearch
  ( RegConfig
  , TSVector
  , TSQuery
  , TSWeight
  , tsMatch
  , tsMatchTSQueryTSVector
  , tsVectorConcat
  , toTSVector
  , toTSQuery
  , plainToTSQuery
  , toTSRank
  , setTSWeight
  , simpleRegConfig
  , englishRegConfig
  , tsWeightToValueExpression
  , tsVectorToValueExpression
  , tsQueryToValueExpression
  , tsWeightA
  , tsWeightB
  , tsWeightC
  , tsWeightD
  ) where

import Orville.PostgreSQL.Expr.BinaryOperator (BinaryOperator, binaryOpExpression, binaryOperator)
import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL value expression that evaluates to a TSVector. This could be a
constant value, a column reference.

'TSVector' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype TSVector
  = TSVector RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Type to represent a SQL value expression that evaluates to a TSQuery. This could be a
constant value, a column reference.

'TSQuery' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype TSQuery
  = TSQuery RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Represents a text search configuration.

'RegConfig' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype RegConfig
  = RegConfig RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | A type to represent the weight to be given to elements of a 'TSVector'

'TSWeight' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom SQL.

  @since 1.1.0.0
-}
newtype TSWeight
  = TSWeight RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

tsMatchOp :: BinaryOperator
tsMatchOp = binaryOperator "@@"

tsVectorConcatOp :: BinaryOperator
tsVectorConcatOp = binaryOperator "||"

{- | Matches a 'TSVector' against a 'TSQuery'. This function is an alias for the '@@' operator.

=== Example:

>>> tsMatch (toTSVector (RawSql.fromString 'document') Nothing) (toTSQuery (RawSql.fromString 'query') Nothing)
-- Produces a boolean expression equivalent to 'toTSVector ... @@ toTSQuery ...'.

@since 1.1.0.0
-}
tsMatch :: TSVector -> TSQuery -> BooleanExpr
tsMatch tsVector tsQuery =
  binaryOpExpression
    tsMatchOp
    (tsVectorToValueExpression tsVector)
    (tsQueryToValueExpression tsQuery)

{- | Matches a 'TSQuery' against a 'TSVector'. This function is an alias for the '@@' operator.
    Filped version of 'tsMatch'

@since 1.1.0.0
-}
tsMatchTSQueryTSVector :: TSQuery -> TSVector -> BooleanExpr
tsMatchTSQueryTSVector = flip tsMatch

{- | Concatenates two 'TSVector' values.

=== Example:

>>> tsVectorConcat (toTSVector (RawSql.fromString 'vector1') Nothing) (toTSVector (RawSql.fromString 'vector2') Nothing)
-- Produces a 'TSVector' equivalent to 'vector1 || vector2'.

@since 1.1.0.0
-}
tsVectorConcat :: TSVector -> TSVector -> TSVector
tsVectorConcat tsVector1 tsVector2 =
  binaryOpExpression
    tsVectorConcatOp
    (tsVectorToValueExpression tsVector1)
    (tsVectorToValueExpression tsVector2)

{- | A constant representing the "A" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightA :: TSWeight
tsWeightA = TSWeight $ RawSql.fromString "\'A\'"

{- | A constant representing the "B" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightB :: TSWeight
tsWeightB = TSWeight $ RawSql.fromString "\'B\'"

{- | A constant representing the "C" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightC :: TSWeight
tsWeightC = TSWeight $ RawSql.fromString "\'C\'"

{- | A constant representing the "D" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightD :: TSWeight
tsWeightD = TSWeight $ RawSql.fromString "\'D\'"

toTSVectorFunction :: FunctionName
toTSVectorFunction = functionName "to_tsvector"

toTSQueryFunction :: FunctionName
toTSQueryFunction = functionName "to_tsquery"

toTSRankFunction :: FunctionName
toTSRankFunction = functionName "ts_rank"

setTSWeightFunction :: FunctionName
setTSWeightFunction = functionName "setweight"

plaintoTSQueryFunction :: FunctionName
plaintoTSQueryFunction = functionName "plainto_tsquery"

{- | A constant representing the "simple" text search configuration.

@since 1.1.0.0
-}
simpleRegConfig :: RegConfig
simpleRegConfig = RegConfig $ RawSql.fromString "simple"

{- | A constant representing the "english" text search configuration.

@since 1.1.0.0
-}
englishRegConfig :: RegConfig
englishRegConfig = RegConfig $ RawSql.fromString "english"

{- | Converts a 'RegConfig' to a 'ValueExpression'.

@since 1.1.0.0
-}
regConfigToValueExpression :: RegConfig -> ValueExpression
regConfigToValueExpression (RegConfig regConfig) =
  RawSql.unsafeFromRawSql
    ( RawSql.fromString "\'"
        <> regConfig
        <> RawSql.fromString "\'"
    )

{- | Converts a 'ValueExpression' to a 'TSVector', optionally using a specified 'RegConfig'.

The provided 'ValueExpression' must adhere to the following limitations:
  * It must evaluate to one of the following types: @text@, @json@, or @jsonb@.
  * Behavior may differ depending on whether @json@ or @jsonb@ is used.
  * Word normalization will be applied during the conversion process.

=== Example:

>>> toTSVector (RawSql.fromString 'text') (Just simpleRegConfig)
-- Produces a 'TSVector' for the given text using the 'simple' configuration.

@since 1.1.0.0
-}
toTSVector :: ValueExpression -> Maybe RegConfig -> TSVector
toTSVector val mbRegConfig = TSVector . RawSql.toRawSql $
  functionCall toTSVectorFunction $ case mbRegConfig of
    Nothing -> [val]
    Just regConfig -> [regConfigToValueExpression regConfig, val]

{- | Converts a 'ValueExpression' to a 'TSQuery', optionally using a specified 'RegConfig'.

The provided 'ValueExpression' must adhere to the following limitations:
  * It must evaluate to one of the following types: @text@, @json@, or @jsonb@.
  * Behavior may differ depending on whether @json@ or @jsonb@ is used.
  * Word normalization will be applied during the conversion process.

=== Example:

>>> toTSQuery (RawSql.fromString 'search term') (Just englishRegConfig)
-- Produces a 'TSQuery' for the given term using the 'english' configuration.

@since 1.1.0.0
-}
toTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
toTSQuery val mbRegConfig = TSQuery . RawSql.toRawSql $
  functionCall toTSQueryFunction $ case mbRegConfig of
    Nothing -> [val]
    Just regConfig -> [regConfigToValueExpression regConfig, val]

{- | Converts a 'TSQuery' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsQueryToValueExpression :: TSQuery -> ValueExpression
tsQueryToValueExpression (TSQuery rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Converts a 'TSVector' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsVectorToValueExpression :: TSVector -> ValueExpression
tsVectorToValueExpression (TSVector rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Converts a 'TSVector' and 'TSQuery' into a rank ('TSRank') based on how well they match.

=== Example:

>>> toTSRank (toTSVector (RawSql.fromString 'text') Nothing) (toTSQuery (RawSql.fromString 'search term') Nothing)
-- Produces a 'TSRank' for the given vector and query.

@since 1.1.0.0
-}
toTSRank :: TSVector -> TSQuery -> ValueExpression
toTSRank tsVector tsQuery =
  functionCall
    toTSRankFunction
    [ tsVectorToValueExpression tsVector
    , tsQueryToValueExpression tsQuery
    ]

{- | Assigns the given weight to each element of a 'TSVector'.

=== Example:

>>> setTSWeight (toTSVector (RawSql.fromString 'text') Nothing) tsWeightA
-- Produces a 'TSVector' with weight 'A'.

@since 1.1.0.0
-}
setTSWeight :: TSVector -> TSWeight -> TSVector
setTSWeight tsVector tsWeight =
  TSVector . RawSql.toRawSql $
    functionCall
      setTSWeightFunction
      [tsVectorToValueExpression tsVector, tsWeightToValueExpression tsWeight]

{- | Converts a 'TSWeight' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsWeightToValueExpression :: TSWeight -> ValueExpression
tsWeightToValueExpression (TSWeight rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Converts a 'ValueExpression' into a 'TSQuery', optionally using a specified 'RegConfig'. 
     The 'ValueExpression' must be text. All punctuation in the given 'ValueExpression' will be ignored 
     and individual words will be combined with a logical AND. This results in a 'TSQuery' 
     that matches when all words are present.

@since 1.1.0.0
-}
plainToTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
plainToTSQuery val mbRegConfig = TSQuery . RawSql.toRawSql $
  functionCall plaintoTSQueryFunction $ case mbRegConfig of
    Nothing -> [val]
    Just regConfig -> [regConfigToValueExpression regConfig, val]
