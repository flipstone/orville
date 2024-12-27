{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024-2025
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.TSVector
  ( RegConfig
  , TSVector
  , TSQuery
  , TSRank
  , TSWeight
  , tsMatch
  , tsMatch_
  , tsVectorConcat
  , toTSVector
  , toTSQuery
  , plainToTSQuery
  , toTSRank
  , setTSWeight
  , simpleRegconfig
  , englishRegconfig
  , toRegconfig
  , tsWeightToValueExpression
  , tsRankToValueExpression
  , tsVectorToValueExpression
  , tsQueryToValueExpression
  , tsWeightA
  , tsWeightB
  , tsWeightC
  , tsWeightD
  , (@@)
  ) where

import Orville.PostgreSQL.Expr.BinaryOperator (BinaryOperator, binaryOpExpression, binaryOperator)
import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL value expression that evaluates to a TSVector. This could be a
constant value, a column reference or any arbitrary calculated expression.

'ValueExpression' provides a 'RawSql.SqlExpression' instance. See
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
constant value, a column reference or any arbitrary calculated expression.

'ValueExpression' provides a 'RawSql.SqlExpression' instance. See
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

{- | Type to represent a SQL value expression that evaluates to a TSRank. This could be a
constant value, a column reference or any arbitrary calculated expression.

'ValueExpression' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

 @since 1.1.0.0
-}
newtype TSRank
  = TSRank RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Represents a text search configuration.

'ValueExpression' provides a 'RawSql.SqlExpression' instance. See
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

{- | A newtype wrapper for a SQL expression that evaluates to a weight for a 'TSVector'.

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
tsMatch_ :: TSQuery -> TSVector -> BooleanExpr
tsMatch_ tsQuery tsVector =
  binaryOpExpression
    tsMatchOp
    (tsQueryToValueExpression tsQuery)
    (tsVectorToValueExpression tsVector)

{- | The SQL @(@@)@ operator (alias for 'tsMatch').

@since 1.1.0.0
-}
(@@) :: TSVector -> TSQuery -> BooleanExpr
(@@) = tsMatch

infixr 8 @@

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
tsWeightA =
  TSWeight $
    RawSql.unsafeFromRawSql
      ( RawSql.fromString "\'"
          <> RawSql.fromString "A"
          <> RawSql.fromString "\'"
      )

{- | A constant representing the "B" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightB :: TSWeight
tsWeightB =
  TSWeight $
    RawSql.unsafeFromRawSql
      ( RawSql.fromString "\'"
          <> RawSql.fromString "B"
          <> RawSql.fromString "\'"
      )

{- | A constant representing the "C" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightC :: TSWeight
tsWeightC =
  TSWeight $
    RawSql.unsafeFromRawSql
      ( RawSql.fromString "\'"
          <> RawSql.fromString "C"
          <> RawSql.fromString "\'"
      )

{- | A constant representing the "D" weight for a 'TSVector'.

@since 1.1.0.0
-}
tsWeightD :: TSWeight
tsWeightD =
  TSWeight $
    RawSql.unsafeFromRawSql
      ( RawSql.fromString "\'"
          <> RawSql.fromString "D"
          <> RawSql.fromString "\'"
      )

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
simpleRegconfig :: RegConfig
simpleRegconfig = toRegconfig "simple"

{- | A constant representing the "english" text search configuration.

@since 1.1.0.0
-}
englishRegconfig :: RegConfig
englishRegconfig = toRegconfig "english"

{- | Converts a string into a 'RegConfig'.

=== Example:

>>> toRegconfig "finnish"
-- Produces a 'RegConfig' for the given configuration name.

@since 1.1.0.0
-}
toRegconfig :: String -> RegConfig
toRegconfig conf = RegConfig $ RawSql.fromString conf

{- | Converts a 'RegConfig' to a 'ValueExpression'.

@since 1.1.0.0
-}
regconfigToValueExpression :: RegConfig -> ValueExpression
regconfigToValueExpression (RegConfig regconfig) =
  RawSql.unsafeFromRawSql $
    ( RawSql.fromString "\'"
        <> regconfig
        <> RawSql.fromString "\'"
    )

{- | Converts a 'ValueExpression' to a 'TSVector', optionally using a specified 'RegConfig'.

=== Example:

>>> toTSVector (RawSql.fromString 'text') (Just simpleRegconfig)
-- Produces a 'TSVector' for the given text using the 'simple' configuration.

@since 1.1.0.0
-}
toTSVector :: ValueExpression -> Maybe RegConfig -> TSVector
toTSVector val mbRegconfig = TSVector . RawSql.toRawSql $
  functionCall toTSVectorFunction $ case mbRegconfig of
    Nothing -> [val]
    Just regconfig -> [regconfigToValueExpression regconfig, val]

{- | Converts a 'ValueExpression' to a 'TSQuery', optionally using a specified 'RegConfig'.

=== Example:

>>> toTSQuery (RawSql.fromString 'search term') (Just englishRegconfig)
-- Produces a 'TSQuery' for the given term using the 'english' configuration.

@since 1.1.0.0
-}
toTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
toTSQuery val mbRegconfig = TSQuery . RawSql.toRawSql $
  functionCall toTSQueryFunction $ case mbRegconfig of
    Nothing -> [val]
    Just regconfig -> [regconfigToValueExpression regconfig, val]

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
toTSRank :: TSVector -> TSQuery -> TSRank
toTSRank tsVector tsQuery =
  TSRank . RawSql.toRawSql $
    functionCall
      toTSRankFunction
      [ tsVectorToValueExpression tsVector
      , tsQueryToValueExpression tsQuery
      ]

{- | Converts a 'TSRank' to a 'ValueExpression'.

@since 1.1.0.0
-}
tsRankToValueExpression :: TSRank -> ValueExpression
tsRankToValueExpression (TSRank rawSql) = RawSql.unsafeFromRawSql rawSql

{- | Assigns a weight to a 'TSVector'.

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

{- | Converts plain text into a 'TSQuery', optionally using a specified 'RegConfig'.

@since 1.1.0.0
-}
plainToTSQuery :: ValueExpression -> Maybe RegConfig -> TSQuery
plainToTSQuery val mbRegconfig = TSQuery . RawSql.toRawSql $
  functionCall plaintoTSQueryFunction $ case mbRegconfig of
    Nothing -> [val]
    Just regconfig -> [regconfigToValueExpression regconfig, val]