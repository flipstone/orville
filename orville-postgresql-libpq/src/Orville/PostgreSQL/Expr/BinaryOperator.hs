{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2023
License   : MIT
-}
module Orville.PostgreSQL.Expr.BinaryOperator
  ( BinaryOperator,
    binaryOperator,
    binaryOpExpression,
    equalsOp,
    notEqualsOp,
    greaterThanOp,
    lessThanOp,
    greaterThanOrEqualsOp,
    lessThanOrEqualsOp,
    likeOp,
    iLikeOp,
    orOp,
    andOp,
    plusOp,
    minusOp,
    multiplicationOp,
    divisionOp,
    moduloOp,
    exponentiationOp,
    bitwiseAndOp,
    bitwiseOrOp,
    bitwiseXorOp,
    bitwiseShiftLeftOp,
    bitwiseShiftRightOp,
  )
where

import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype BinaryOperator
  = BinaryOperator RawSql.RawSql
  deriving (RawSql.SqlExpression)

binaryOperator :: String -> BinaryOperator
binaryOperator =
  BinaryOperator . RawSql.fromString

equalsOp :: BinaryOperator
equalsOp =
  binaryOperator "="

notEqualsOp :: BinaryOperator
notEqualsOp =
  binaryOperator "<>"

greaterThanOp :: BinaryOperator
greaterThanOp =
  binaryOperator ">"

lessThanOp :: BinaryOperator
lessThanOp =
  binaryOperator "<"

greaterThanOrEqualsOp :: BinaryOperator
greaterThanOrEqualsOp =
  binaryOperator ">="

lessThanOrEqualsOp :: BinaryOperator
lessThanOrEqualsOp =
  binaryOperator "<="

likeOp :: BinaryOperator
likeOp =
  binaryOperator "LIKE"

iLikeOp :: BinaryOperator
iLikeOp =
  binaryOperator "ILIKE"

orOp :: BinaryOperator
orOp =
  binaryOperator "OR"

andOp :: BinaryOperator
andOp =
  binaryOperator "AND"

plusOp :: BinaryOperator
plusOp =
  binaryOperator "+"

minusOp :: BinaryOperator
minusOp =
  binaryOperator "-"

multiplicationOp :: BinaryOperator
multiplicationOp =
  binaryOperator "*"

divisionOp :: BinaryOperator
divisionOp =
  binaryOperator "/"

moduloOp :: BinaryOperator
moduloOp =
  binaryOperator "%"

exponentiationOp :: BinaryOperator
exponentiationOp =
  binaryOperator "^"

bitwiseAndOp :: BinaryOperator
bitwiseAndOp =
  binaryOperator "&"

bitwiseOrOp :: BinaryOperator
bitwiseOrOp =
  binaryOperator "|"

bitwiseXorOp :: BinaryOperator
bitwiseXorOp =
  binaryOperator "#"

bitwiseShiftLeftOp :: BinaryOperator
bitwiseShiftLeftOp =
  binaryOperator "<<"

bitwiseShiftRightOp :: BinaryOperator
bitwiseShiftRightOp =
  binaryOperator ">>"

binaryOpExpression ::
  RawSql.SqlExpression sql =>
  BinaryOperator ->
  ValueExpression ->
  ValueExpression ->
  sql
binaryOpExpression op left right =
  binaryOpExpressionUnparenthenizedArguments
    op
    (RawSql.unsafeFromRawSql (RawSql.leftParen <> RawSql.toRawSql left <> RawSql.rightParen))
    (RawSql.unsafeFromRawSql (RawSql.leftParen <> RawSql.toRawSql right <> RawSql.rightParen))

binaryOpExpressionUnparenthenizedArguments ::
  RawSql.SqlExpression sql =>
  BinaryOperator ->
  ValueExpression ->
  ValueExpression ->
  sql
binaryOpExpressionUnparenthenizedArguments op left right =
  RawSql.unsafeFromRawSql $
    RawSql.toRawSql left
      <> RawSql.space
      <> RawSql.toRawSql op
      <> RawSql.space
      <> RawSql.toRawSql right
