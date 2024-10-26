{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Provides a type representing SQL operators with exactly two arguments, as well
as values of that type for many common operators.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.BinaryOperator
  ( BinaryOperator
  , binaryOperator
  , binaryOpExpression
  , equalsOp
  , isDistinctFromOp
  , isNotDistinctFromOp
  , notEqualsOp
  , greaterThanOp
  , lessThanOp
  , greaterThanOrEqualsOp
  , lessThanOrEqualsOp
  , likeOp
  , iLikeOp
  , orOp
  , andOp
  , plusOp
  , minusOp
  , multiplicationOp
  , divisionOp
  , moduloOp
  , exponentiationOp
  , bitwiseAndOp
  , bitwiseOrOp
  , bitwiseXorOp
  , bitwiseShiftLeftOp
  , bitwiseShiftRightOp
  )
where

import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent any SQL operator of two arguments. E.G.

> AND

'BinaryOperator' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype BinaryOperator
  = BinaryOperator RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Construct a binary operator. Note that this does not include any check to determine if the
operator is valid, either by being a native SQL operator, or a custom-defined operator in the
database.

@since 1.0.0.0
-}
binaryOperator :: String -> BinaryOperator
binaryOperator =
  BinaryOperator . RawSql.fromString

{- | The SQL equal binary operator.

@since 1.0.0.0
-}
equalsOp :: BinaryOperator
equalsOp =
  binaryOperator "="

{- | The SQL not equal binary operator.

@since 1.0.0.0
-}
notEqualsOp :: BinaryOperator
notEqualsOp =
  binaryOperator "<>"

{- | The SQL 'IS DISTINCT FROM' binary 'operator'.

@since 1.1.0.0
-}
isDistinctFromOp :: BinaryOperator
isDistinctFromOp = binaryOperator "IS DISTINCT FROM"

{- | The SQL 'IS NOT DISTINCT FROM' binary 'operator'.

@since 1.1.0.0
-}
isNotDistinctFromOp :: BinaryOperator
isNotDistinctFromOp = binaryOperator "IS NOT DISTINCT FROM"

{- | The SQL strictly greater than binary operator.

@since 1.0.0.0
-}
greaterThanOp :: BinaryOperator
greaterThanOp =
  binaryOperator ">"

{- | The SQL strictly less than binary operator.

@since 1.0.0.0
-}
lessThanOp :: BinaryOperator
lessThanOp =
  binaryOperator "<"

{- | The SQL greater than or equal binary operator.

@since 1.0.0.0
-}
greaterThanOrEqualsOp :: BinaryOperator
greaterThanOrEqualsOp =
  binaryOperator ">="

{- | The SQL less than or equal binary operator.

@since 1.0.0.0
-}
lessThanOrEqualsOp :: BinaryOperator
lessThanOrEqualsOp =
  binaryOperator "<="

{- | The SQL LIKE binary operator.

@since 1.0.0.0
-}
likeOp :: BinaryOperator
likeOp =
  binaryOperator "LIKE"

{- | The SQL ILIKE binary operator.

@since 1.0.0.0
-}
iLikeOp :: BinaryOperator
iLikeOp =
  binaryOperator "ILIKE"

{- | The SQL logical or binary operator.

@since 1.0.0.0
-}
orOp :: BinaryOperator
orOp =
  binaryOperator "OR"

{- | The SQL logical and binary operator.

@since 1.0.0.0
-}
andOp :: BinaryOperator
andOp =
  binaryOperator "AND"

{- | The SQL + binary operator.

@since 1.0.0.0
-}
plusOp :: BinaryOperator
plusOp =
  binaryOperator "+"

{- | The SQL - binary operator.

@since 1.0.0.0
-}
minusOp :: BinaryOperator
minusOp =
  binaryOperator "-"

{- | The SQL * binary operator.

@since 1.0.0.0
-}
multiplicationOp :: BinaryOperator
multiplicationOp =
  binaryOperator "*"

{- | The SQL / binary operator.

@since 1.0.0.0
-}
divisionOp :: BinaryOperator
divisionOp =
  binaryOperator "/"

{- | The SQL % binary operator.

@since 1.0.0.0
-}
moduloOp :: BinaryOperator
moduloOp =
  binaryOperator "%"

{- | The SQL ^ binary operator.

@since 1.0.0.0
-}
exponentiationOp :: BinaryOperator
exponentiationOp =
  binaryOperator "^"

{- | The SQL bitwise and (a.k.a &) binary operator.

@since 1.0.0.0
-}
bitwiseAndOp :: BinaryOperator
bitwiseAndOp =
  binaryOperator "&"

{- | The SQL bitwise or (a.k.a |) binary operator.

@since 1.0.0.0
-}
bitwiseOrOp :: BinaryOperator
bitwiseOrOp =
  binaryOperator "|"

{- | The SQL bitwise exclusive or (a.k.a #) binary operator.

@since 1.0.0.0
-}
bitwiseXorOp :: BinaryOperator
bitwiseXorOp =
  binaryOperator "#"

{- | The SQL bitwise left shift (a.k.a <<) binary operator.

@since 1.0.0.0
-}
bitwiseShiftLeftOp :: BinaryOperator
bitwiseShiftLeftOp =
  binaryOperator "<<"

{- | The SQL bitwise right shift (a.k.a >>) binary operator.

@since 1.0.0.0
-}
bitwiseShiftRightOp :: BinaryOperator
bitwiseShiftRightOp =
  binaryOperator ">>"

{- | Apply a binary operator to two 'ValueExpression's resulting in some
 'RawSql.SqlExpression'. Note that this does *NOT* extend typechecking to the 'ValueExpression's
 being used with the 'BinaryOperator'. It is left to the caller to ensure that the operator makes
 sense with the arguments being passed.

@since 1.0.0.0
-}
binaryOpExpression ::
  RawSql.SqlExpression sql =>
  BinaryOperator ->
  ValueExpression ->
  ValueExpression ->
  sql
binaryOpExpression op left right =
  binaryOpExpressionUnparenthenizedArguments
    op
    (RawSql.unsafeFromRawSql (RawSql.parenthesized left))
    (RawSql.unsafeFromRawSql (RawSql.parenthesized right))

-- internal helper function
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
