{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.WhereClause
  ( WhereClause
  , whereClause
  , BooleanExpr
  , literalBooleanExpr
  , andExpr
  , (.&&)
  , orExpr
  , (.||)
  , notExpr
  , parenthesized
  , equals
  , notEquals
  , isDistinctFrom
  , isNotDistinctFrom
  , greaterThan
  , lessThan
  , greaterThanOrEqualTo
  , lessThanOrEqualTo
  , like
  , likeInsensitive
  , isNull
  , isNotNull
  , valueIn
  , valueNotIn
  , tupleIn
  , tupleNotIn
  , InValuePredicate
  , inPredicate
  , notInPredicate
  , inValueList
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.BinaryOperator (andOp, binaryOpExpression, equalsOp, greaterThanOp, greaterThanOrEqualsOp, iLikeOp, isDistinctFromOp, isNotDistinctFromOp, lessThanOp, lessThanOrEqualsOp, likeOp, notEqualsOp, orOp)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, rowValueConstructor)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a @WHERE@ clause restriction on a @SELECT@, @UPDATE@ or
@DELETE@ statement. E.G.

> WHERE (foo > 10)

'WhereClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype WhereClause
  = WhereClause RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
Constructs a @WHERE@ clause from the given 'BooleanExpr'. E.G.

> WHERE <boolean expr>

@since 1.0.0.0
-}
whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    RawSql.fromString "WHERE " <> RawSql.toRawSql booleanExpr

{- |
Type to represent a SQL value expression that evaluates to a boolean and therefore
can used with boolean logic functions. E.G.

> foo > 10

'BooleanExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype BooleanExpr
  = BooleanExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs a 'BooleanExpr' whose value is the SQL literal @TRUE@ or @FALSE@
  depending on the argument given.

  @since 1.0.0.0
-}
literalBooleanExpr :: Bool -> BooleanExpr
literalBooleanExpr bool =
  BooleanExpr . RawSql.fromString $
    case bool of
      False -> "FALSE"
      True -> "TRUE"

{- |
  Converts a 'BooleanExpr' to a 'ValueExpression' so that it can be used
  anywhere 'ValueExpression' is allowed.

  @since 1.0.0.0
-}
booleanValueExpression :: BooleanExpr -> ValueExpression
booleanValueExpression (BooleanExpr rawSql) =
  RawSql.unsafeFromRawSql rawSql

{- |
  The SQL @OR@ operator. The arguments will be surrounded with parentheses
  to ensure that the associativity of expression in the resulting SQL matches
  the associativity implied by this Haskell function.

  @since 1.0.0.0
-}
orExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
orExpr left right =
  binaryOpExpression
    orOp
    (booleanValueExpression left)
    (booleanValueExpression right)

{- |
  The SQL @OR@ operator (alias for 'orExpr').

  @since 1.0.0.0
-}
(.||) :: BooleanExpr -> BooleanExpr -> BooleanExpr
(.||) = orExpr

infixr 8 .||

{- |
  The SQL @AND@ operator. The arguments will be surrounded with parentheses
  to ensure that the associativity of expression in the resulting SQL matches
  the associativity implied by this Haskell function.

  @since 1.0.0.0
-}
andExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
andExpr left right =
  binaryOpExpression
    andOp
    (booleanValueExpression left)
    (booleanValueExpression right)

{- |
  The SQL @AND@ operator (alias for 'andExpr').

  @since 1.0.0.0
-}
(.&&) :: BooleanExpr -> BooleanExpr -> BooleanExpr
(.&&) = andExpr

infixr 8 .&&

{- |
  The SQL @NOT@ operator. The argument will be surrounded with parentheses
  to ensure that the associativity of expression in the resulting SQL matches
  the associativity implied by this Haskell function.

  @since 1.1.0.0
-}
notExpr :: BooleanExpr -> BooleanExpr
notExpr bool =
  BooleanExpr $
    RawSql.fromString "NOT "
      <> RawSql.parenthesized bool

{- |
  The SQL @IN@ operator. The result will be @TRUE@ if the given value
  appears in the list of values given.

  @since 1.0.0.0
-}
valueIn :: ValueExpression -> NE.NonEmpty ValueExpression -> BooleanExpr
valueIn needle haystack =
  inPredicate needle (inValueList haystack)

{- |
  The SQL @NOT IN@ operator. The result will be @TRUE@ if the given value
  does not appear in the list of values given.

  @since 1.0.0.0
-}
valueNotIn :: ValueExpression -> NE.NonEmpty ValueExpression -> BooleanExpr
valueNotIn needle haystack =
  notInPredicate needle (inValueList haystack)

{- |
  The SQL @IN@ operator, like 'valueIn', but for when you want to construct a
  tuple in SQL and check if it is in a list of tuples. It is up to the caller
  to ensure that all the tuples given have the same arity.

  @since 1.0.0.0
-}
tupleIn :: NE.NonEmpty ValueExpression -> NE.NonEmpty (NE.NonEmpty ValueExpression) -> BooleanExpr
tupleIn needle haystack =
  inPredicate
    (rowValueConstructor needle)
    (inValueList (fmap rowValueConstructor haystack))

{- |
  The SQL @NOT IN@ operator, like 'valueNotIn', but for when you want to
  construct a tuple in SQL and check if it is not in a list of tuples. It is up
  to the caller to ensure that all the tuples given have the same arity.

  @since 1.0.0.0
-}
tupleNotIn :: NE.NonEmpty ValueExpression -> NE.NonEmpty (NE.NonEmpty ValueExpression) -> BooleanExpr
tupleNotIn needle haystack =
  notInPredicate
    (rowValueConstructor needle)
    (inValueList (fmap rowValueConstructor haystack))

{- |
  Lower-level access to the SQL @IN@ operator. This takes any 'ValueExpression'
  and 'InValuePredicate'. It is up to the caller to ensure the expressions
  given make sense together.

  @since 1.0.0.0
-}
inPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
inPredicate predicand predicate =
  BooleanExpr $
    RawSql.parenthesized predicand
      <> RawSql.fromString " IN "
      <> RawSql.toRawSql predicate

{- |
  Lower-level access to the SQL @NOT IN@ operator. This takes any
  'ValueExpression' and 'InValuePredicate'. It is up to the caller to ensure
  the expressions given make sense together.

  @since 1.0.0.0
-}
notInPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
notInPredicate predicand predicate =
  BooleanExpr $
    RawSql.parenthesized predicand
      <> RawSql.fromString " NOT IN "
      <> RawSql.toRawSql predicate

{- |
Type to represent the right hand side of an @IN@ or @NOT IN@ expression.
E.G.

> (10,12,13)

'InValuePredicate' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype InValuePredicate
  = InValuePredicate RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs an 'InValuePredicate' from the given list of 'ValueExpression'.

  @since 1.0.0.0
-}
inValueList :: NE.NonEmpty ValueExpression -> InValuePredicate
inValueList values =
  InValuePredicate $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.commaSpace values
      <> RawSql.rightParen

{- |
  Surrounds the given 'BooleanExpr' with parentheses.

  @since 1.0.0.0
-}
parenthesized :: BooleanExpr -> BooleanExpr
parenthesized expr =
  BooleanExpr $
    RawSql.leftParen <> RawSql.toRawSql expr <> RawSql.rightParen

{- |
  The SQL @=@ operator.

  @since 1.0.0.0
-}
equals :: ValueExpression -> ValueExpression -> BooleanExpr
equals =
  binaryOpExpression equalsOp

{- |
  The SQL @<>@ operator.

  @since 1.0.0.0
-}
notEquals :: ValueExpression -> ValueExpression -> BooleanExpr
notEquals =
  binaryOpExpression notEqualsOp

{- |
  The SQL @IS DISTINCT FROM@ binary comparison.

  @since 1.1.0.0
-}
isDistinctFrom :: ValueExpression -> ValueExpression -> BooleanExpr
isDistinctFrom =
  binaryOpExpression isDistinctFromOp

{- |
  The SQL @IS NOT DISTINCT FROM@ binary comparison.

  @since 1.1.0.0
-}
isNotDistinctFrom :: ValueExpression -> ValueExpression -> BooleanExpr
isNotDistinctFrom =
  binaryOpExpression isNotDistinctFromOp

{- |
  The SQL @>@ operator.

  @since 1.0.0.0
-}
greaterThan :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThan =
  binaryOpExpression greaterThanOp

{- |
  The SQL @<@ operator.

  @since 1.0.0.0
-}
lessThan :: ValueExpression -> ValueExpression -> BooleanExpr
lessThan =
  binaryOpExpression lessThanOp

{- |
  The SQL @>=@ operator.

  @since 1.0.0.0
-}
greaterThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
greaterThanOrEqualTo =
  binaryOpExpression greaterThanOrEqualsOp

{- |
  The SQL @<=@ operator.

  @since 1.0.0.0
-}
lessThanOrEqualTo :: ValueExpression -> ValueExpression -> BooleanExpr
lessThanOrEqualTo =
  binaryOpExpression lessThanOrEqualsOp

{- |
  The SQL @LIKE@ operator.

  @since 1.0.0.0
-}
like :: ValueExpression -> ValueExpression -> BooleanExpr
like =
  binaryOpExpression likeOp

{- |
  The SQL @ILIKE@ operator.

  @since 1.0.0.0
-}
likeInsensitive :: ValueExpression -> ValueExpression -> BooleanExpr
likeInsensitive =
  binaryOpExpression iLikeOp

{- |
  The SQL @IS NULL@ condition.

  @since 1.0.0.0
-}
isNull :: ValueExpression -> BooleanExpr
isNull value =
  BooleanExpr $
    RawSql.toRawSql value
      <> RawSql.space
      <> RawSql.fromString "IS NULL"

{- |
  The SQL @IS NOT NULL@ condition.

  @since 1.0.0.0
-}
isNotNull :: ValueExpression -> BooleanExpr
isNotNull value =
  BooleanExpr $
    RawSql.toRawSql value
      <> RawSql.space
      <> RawSql.fromString "IS NOT NULL"
