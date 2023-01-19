{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.BooleanExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.BooleanExpr
  ( BooleanExpr,
    orExpr,
    andExpr,
    parenthesized,
    equals,
    notEquals,
    greaterThan,
    lessThan,
    greaterThanOrEqualTo,
    lessThanOrEqualTo,
    like,
    likeInsensitive,
    isNull,
    isNotNull,
    comparison,
    valueIn,
    valueNotIn,
    tupleIn,
    tupleNotIn,
    InValuePredicate,
    inPredicate,
    notInPredicate,
    inValueList,
  )
where

import qualified Data.List.NonEmpty as NE
import Orville.PostgreSQL.Internal.Expr.ValueExpression (ValueExpression, rowValueConstructor)
import Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator (ComparisonOperator, equalsOp, greaterThanOp, greaterThanOrEqualsOp, iLikeOp, lessThanOp, lessThanOrEqualsOp, likeOp, notEqualsOp)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype BooleanExpr
  = BooleanExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

orExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
orExpr left right =
  BooleanExpr $
    RawSql.toRawSql left
      <> RawSql.fromString " OR "
      <> RawSql.toRawSql right

andExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
andExpr left right =
  BooleanExpr $
    RawSql.toRawSql left
      <> RawSql.fromString " AND "
      <> RawSql.toRawSql right

valueIn :: ValueExpression -> NE.NonEmpty ValueExpression -> BooleanExpr
valueIn needle haystack =
  inPredicate needle (inValueList haystack)

valueNotIn :: ValueExpression -> NE.NonEmpty ValueExpression -> BooleanExpr
valueNotIn needle haystack =
  notInPredicate needle (inValueList haystack)

{- |
  Checks that the tuple constructed from the given values in one of the tuples
  specified in the input list. It is up to the caller to ensure that all the
  tuples given have the same arity.
-}
tupleIn :: NE.NonEmpty ValueExpression -> NE.NonEmpty (NE.NonEmpty ValueExpression) -> BooleanExpr
tupleIn needle haystack =
  inPredicate
    (rowValueConstructor needle)
    (inValueList (fmap rowValueConstructor haystack))

{- |
  Checks that the tuple constructed from the given values is NOT one of the
  tuples specified in the input list. It is up to the caller to ensure that all
  the tuples given have the same arity.
-}
tupleNotIn :: NE.NonEmpty ValueExpression -> NE.NonEmpty (NE.NonEmpty ValueExpression) -> BooleanExpr
tupleNotIn needle haystack =
  notInPredicate
    (rowValueConstructor needle)
    (inValueList (fmap rowValueConstructor haystack))

inPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
inPredicate predicand predicate =
  BooleanExpr $
    RawSql.toRawSql predicand
      <> RawSql.fromString " IN "
      <> RawSql.toRawSql predicate

notInPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
notInPredicate predicand predicate =
  BooleanExpr $
    RawSql.toRawSql predicand
      <> RawSql.fromString " NOT IN "
      <> RawSql.toRawSql predicate

newtype InValuePredicate
  = InValuePredicate RawSql.RawSql
  deriving (RawSql.SqlExpression)

inValueList :: NE.NonEmpty ValueExpression -> InValuePredicate
inValueList values =
  InValuePredicate $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.commaSpace values
      <> RawSql.rightParen

parenthesized :: BooleanExpr -> BooleanExpr
parenthesized expr =
  BooleanExpr $
    RawSql.leftParen <> RawSql.toRawSql expr <> RawSql.rightParen

equals :: ValueExpression -> ValueExpression -> BooleanExpr
equals =
  comparison equalsOp

notEquals :: ValueExpression -> ValueExpression -> BooleanExpr
notEquals =
  comparison notEqualsOp

greaterThan :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThan =
  comparison greaterThanOp

lessThan :: ValueExpression -> ValueExpression -> BooleanExpr
lessThan =
  comparison lessThanOp

greaterThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThanOrEqualTo =
  comparison greaterThanOrEqualsOp

lessThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
lessThanOrEqualTo =
  comparison lessThanOrEqualsOp

like :: ValueExpression -> ValueExpression -> BooleanExpr
like =
  comparison likeOp

likeInsensitive :: ValueExpression -> ValueExpression -> BooleanExpr
likeInsensitive =
  comparison iLikeOp

comparison ::
  ComparisonOperator ->
  ValueExpression ->
  ValueExpression ->
  BooleanExpr
comparison op left right =
  BooleanExpr $
    RawSql.toRawSql left
      <> RawSql.space
      <> RawSql.toRawSql op
      <> RawSql.space
      <> RawSql.toRawSql right

isNull :: ValueExpression -> BooleanExpr
isNull value =
  BooleanExpr $
    RawSql.toRawSql value
      <> RawSql.space
      <> RawSql.fromString "IS NULL"

isNotNull :: ValueExpression -> BooleanExpr
isNotNull value =
  BooleanExpr $
    RawSql.toRawSql value
      <> RawSql.space
      <> RawSql.fromString "IS NOT NULL"
