{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.WhereClause
  ( WhereClause,
    whereClause,
    BooleanExpr,
    andExpr,
    (.&&),
    orExpr,
    (.||),
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

import Orville.PostgreSQL.Expr.BinaryOperator (andOp, binaryOpExpression, equalsOp, greaterThanOp, greaterThanOrEqualsOp, iLikeOp, lessThanOp, lessThanOrEqualsOp, likeOp, notEqualsOp, orOp)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, rowValueConstructor)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype WhereClause
  = WhereClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    RawSql.fromString "WHERE " <> RawSql.toRawSql booleanExpr

newtype BooleanExpr
  = BooleanExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

booleanValueExpression :: BooleanExpr -> ValueExpression
booleanValueExpression (BooleanExpr rawSql) =
  RawSql.unsafeFromRawSql rawSql

orExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
orExpr left right =
  binaryOpExpression
    orOp
    (booleanValueExpression left)
    (booleanValueExpression right)

{- |
  Operator alias for 'orExpr'
-}
(.||) :: BooleanExpr -> BooleanExpr -> BooleanExpr
(.||) = orExpr

infixr 8 .||

andExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
andExpr left right =
  binaryOpExpression
    andOp
    (booleanValueExpression left)
    (booleanValueExpression right)

{- |
  Operator alias for 'andExpr'
-}
(.&&) :: BooleanExpr -> BooleanExpr -> BooleanExpr
(.&&) = andExpr

infixr 8 .&&

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
    RawSql.parenthesized predicand
      <> RawSql.fromString " IN "
      <> RawSql.toRawSql predicate

notInPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
notInPredicate predicand predicate =
  BooleanExpr $
    RawSql.parenthesized predicand
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
  binaryOpExpression equalsOp

notEquals :: ValueExpression -> ValueExpression -> BooleanExpr
notEquals =
  binaryOpExpression notEqualsOp

greaterThan :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThan =
  binaryOpExpression greaterThanOp

lessThan :: ValueExpression -> ValueExpression -> BooleanExpr
lessThan =
  binaryOpExpression lessThanOp

greaterThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThanOrEqualTo =
  binaryOpExpression greaterThanOrEqualsOp

lessThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
lessThanOrEqualTo =
  binaryOpExpression lessThanOrEqualsOp

like :: ValueExpression -> ValueExpression -> BooleanExpr
like =
  binaryOpExpression likeOp

likeInsensitive :: ValueExpression -> ValueExpression -> BooleanExpr
likeInsensitive =
  binaryOpExpression iLikeOp

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
