{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.ConditionalExpr
  ( coalesce
  , WhenExpr
  , whenExpr
  , caseExpr
  , nullIf
  , greatest
  , least
  ) where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Creates a 'ValueExpression' corresponding to the SQL @COALESE@.

@since 1.1.0.0
-}
coalesce :: NE.NonEmpty ValueExpression -> ValueExpression
coalesce valExprs =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "COALESCE"
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql valExprs)
      <> RawSql.rightParen

{- |
Type to represent the @WHEN@ portion of a SQL @CASE@ expressions.
E.G.

> WHEN condition THEN result

'WhenExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype WhenExpr = WhenExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
Builds a 'WhenExpr' that will apply when the given 'BooleanExpr' evaluates to @TRUE@, resulting in the 'ValueExpression'

@since 1.1.0.0
-}
whenExpr :: BooleanExpr -> ValueExpression -> WhenExpr
whenExpr boolExpr resultExpr =
  WhenExpr $
    RawSql.fromString "WHEN"
      <> RawSql.space
      <> RawSql.toRawSql boolExpr
      <> RawSql.space
      <> RawSql.fromString "THEN"
      <> RawSql.space
      <> RawSql.toRawSql resultExpr

{- |
Builds a 'ValueExpression' corresponding to a SQL @CASE@ using the given 'WhenExpr' as the tested value with results and an optional 'ValueExpression' that corresponds to the @ELSE@ portion of the @CASE@.

@since 1.1.0.0
-}
caseExpr ::
  NE.NonEmpty WhenExpr ->
  Maybe ValueExpression ->
  ValueExpression
caseExpr whens mbElse =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "CASE"
      <> RawSql.space
      <> RawSql.intercalate RawSql.space (fmap RawSql.toRawSql whens)
      <> RawSql.space
      <> case mbElse of
        Nothing ->
          RawSql.fromString "END"
        Just elseVal ->
          RawSql.fromString "ELSE"
            <> RawSql.space
            <> RawSql.toRawSql elseVal
            <> RawSql.space
            <> RawSql.fromString "END"

{- | Creates a 'ValueExpression' corresponding to the SQL @NULLIF@.

@since 1.1.0.0
-}
nullIf :: ValueExpression -> ValueExpression -> ValueExpression
nullIf leftVal rightVal =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "NULLIF"
      <> RawSql.leftParen
      <> RawSql.toRawSql leftVal
      <> RawSql.commaSpace
      <> RawSql.toRawSql rightVal
      <> RawSql.rightParen

{- | Creates a 'ValueExpression' corresponding to the SQL @GREATEST@.

@since 1.1.0.0
-}
greatest :: NE.NonEmpty ValueExpression -> ValueExpression
greatest valExprs =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "GREATEST"
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql valExprs)
      <> RawSql.rightParen

{- | Creates a 'ValueExpression' corresponding to the SQL @LEAST@.

@since 1.1.0.0
-}
least :: NE.NonEmpty ValueExpression -> ValueExpression
least valExprs =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "LEAST"
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql valExprs)
      <> RawSql.rightParen
