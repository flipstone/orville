{-# LANGUAGE FunctionalDependencies #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Marshall.SqlComparable
  ( SqlComparable (toComparableSqlValue, referenceValueExpression)
  , toSqlValueTuple
  , equals
  , notEquals
  , isDistinctFrom
  , isNotDistinctFrom
  , lessThan
  , lessThanOrEqualTo
  , greaterThan
  , greaterThanOrEqualTo
  , isIn
  , isNotIn
  , isNull
  , isNotNull
  , like
  , likeInsensitive
  , tupleIn
  , tupleNotIn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | This provides a common interface to being able to produce a 'SqlValue.SqlValue' as well as having
a way to reference the item in SQL. This can be something like a field of a table, a field that
should be referenced via an alias, or a subquery.

Here the first argument to the class should be some abstraction over some part of SQL that produces
a Haskell value. With the type of the Haskell value as the second argument.

@since 1.1.0.0
-}
class SqlComparable sqlAbstraction haskellValue | sqlAbstraction -> haskellValue where
  -- | Uses the first argument to marshall a Haskell value its 'SqlValue.SqlValue' representation.
  --
  -- @since 1.1.0.0
  toComparableSqlValue :: sqlAbstraction -> haskellValue -> SqlValue.SqlValue

  -- | Constructs the 'Expr.ValueExpression' for use in SQL expressions from the
  -- "Orville.PostgreSQL.Expr" module.
  --
  -- @since 1.1.0.0
  referenceValueExpression :: sqlAbstraction -> Expr.ValueExpression

{- | Constructs a SqlValue "tuple" (i.e. NonEmpty list) for two items that can themselves be made to 'SqlValue.SqlValue'.

@since 1.1.0.0
-}
toSqlValueTuple ::
  (SqlComparable a c, SqlComparable b d) =>
  a ->
  b ->
  (c, d) ->
  NonEmpty Expr.ValueExpression
toSqlValueTuple a b (c, d) =
  (Expr.valueExpression $ toComparableSqlValue a c)
    :| [Expr.valueExpression $ toComparableSqlValue b d]

{- | Checks that the referenced item from the first argument does not equal the SQL value of the
  second argument. This is commonly used in a @WHERE@ clause such as @WHERE foo = 1@. Frequently you
  would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldEquals'.

@since 1.1.0.0
-}
equals :: SqlComparable a b => a -> b -> Expr.BooleanExpr
equals a =
  Expr.equals
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument does not equal the SQL value of the
  second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo != 1@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldNotEquals'.

@since 1.1.0.0
-}
notEquals :: SqlComparable a b => a -> b -> Expr.BooleanExpr
notEquals a =
  Expr.notEquals
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is distinct from the SQL value of the
  second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo IS DISTINCT FROM 1@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldIsDistinctFrom'.

@since 1.1.0.0
-}
isDistinctFrom :: SqlComparable a b => a -> b -> Expr.BooleanExpr
isDistinctFrom a =
  Expr.isDistinctFrom
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is not distinct from the SQL value of the
  second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo IS NOT DISTINCT FROM 1@. Frequently you would not want to use this directly, but instead
  use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldIsNotDistinctFrom'.

@since 1.1.0.0
-}
isNotDistinctFrom :: SqlComparable a b => a -> b -> Expr.BooleanExpr
isNotDistinctFrom a =
  Expr.isNotDistinctFrom
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is less than the SQL value of the
  second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo < 1@. Frequently you would not want to use this directly, but instead
  use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldLessThan'.

@since 1.1.0.0
-}
lessThan :: SqlComparable a b => a -> b -> Expr.BooleanExpr
lessThan a =
  Expr.lessThan
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is less than or equal to the SQL value of
  the second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo <= 1@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldLessThanOrEqualTo'.

@since 1.1.0.0
-}
lessThanOrEqualTo :: SqlComparable a b => a -> b -> Expr.BooleanExpr
lessThanOrEqualTo a =
  Expr.lessThanOrEqualTo
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is greater than the SQL value of the
  second argument. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE foo > 1@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldGreaterThan'.

@since 1.1.0.0
-}
greaterThan :: SqlComparable a b => a -> b -> Expr.BooleanExpr
greaterThan a =
  Expr.greaterThan
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is greater than or equal to the SQL value
  of the second argument. This is commonly used with columns as you might in a @WHERE@ clause such
  as @WHERE foo >= 1@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldGreaterThanOrEqualTo'.

@since 1.1.0.0
-}
greaterThanOrEqualTo :: SqlComparable a b => a -> b -> Expr.BooleanExpr
greaterThanOrEqualTo a =
  Expr.greaterThanOrEqualTo
    (referenceValueExpression a)
    . Expr.valueExpression
    . toComparableSqlValue a

{- | Checks that the referenced item from the first argument is in the list of the SQL values
  of the second argument. This is commonly used with columns as you might in a @WHERE@ clause such
  as @WHERE foo IN (1,2)@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldIn'.

@since 1.1.0.0
-}
isIn :: SqlComparable a b => a -> NonEmpty b -> Expr.BooleanExpr
isIn a =
  Expr.valueIn
    (referenceValueExpression a)
    . fmap (Expr.valueExpression . toComparableSqlValue a)

{- | Checks that the referenced item from the first argument is not in the list of the SQL values
  of the second argument. This is commonly used with columns as you might in a @WHERE@ clause such
  as @WHERE foo NOT IN (1,2)@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldNotIn'.

@since 1.1.0.0
-}
isNotIn :: SqlComparable a b => a -> NonEmpty b -> Expr.BooleanExpr
isNotIn a =
  Expr.valueNotIn
    (referenceValueExpression a)
    . fmap (Expr.valueExpression . toComparableSqlValue a)

{- | Checks that the referenced item is null. This is commonly used with columns as you might in a
  @WHERE@ clause such as @WHERE foo IS NULL@. Frequently you would not want to use this directly,
  but instead use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldIsNull'.

@since 1.1.0.0
-}
isNull :: SqlComparable a b => a -> Expr.BooleanExpr
isNull =
  Expr.isNull . referenceValueExpression

{- | Checks that the referenced item is not null. This is commonly used with columns as you might in a
  @WHERE@ clause such as @WHERE foo IS NOT NULL@. Frequently you would not want to use this
  directly, but instead use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldIsNotNull'.

@since 1.1.0.0
-}
isNotNull :: SqlComparable a b => a -> Expr.BooleanExpr
isNotNull =
  Expr.isNotNull . referenceValueExpression

{- | Checks that the referenced item matches the given like pattern case sensitively. This is
  commonly used with columns as you might in a @WHERE@ clause such as @WHERE foo LIKE
  'bar'@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldLike'.

@since 1.1.0.0
-}
like :: SqlComparable a b => a -> T.Text -> Expr.BooleanExpr
like a =
  Expr.like
    (referenceValueExpression a)
    . Expr.valueExpression
    . SqlValue.fromText

{- | Checks that the referenced item matches the given like pattern case insensitively. This is
  commonly used with columns as you might in a @WHERE@ clause such as @WHERE foo ILIKE
  'bar'@. Frequently you would not want to use this directly, but instead use
  'Orville.PostgreSQL.Marshall.FieldDefinition.fieldLikeInsensitive'.

@since 1.1.0.0
-}
likeInsensitive :: SqlComparable a b => a -> T.Text -> Expr.BooleanExpr
likeInsensitive a =
  Expr.likeInsensitive
    (referenceValueExpression a)
    . Expr.valueExpression
    . SqlValue.fromText

{- | Checks that the referenced items from the first two arguments are in the SQL values of the
  given list of tuples. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE (foo, bar) IN ((1,2), (3,4))@. Frequently you would not want to use this directly, but
  instead use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldTupleIn'.

@since 1.1.0.0
-}
tupleIn ::
  (SqlComparable a c, SqlComparable b d) =>
  a ->
  b ->
  NonEmpty (c, d) ->
  Expr.BooleanExpr
tupleIn a b =
  Expr.tupleIn
    (referenceValueExpression a :| [referenceValueExpression b])
    . fmap (toSqlValueTuple a b)

{- | Checks that the referenced items from the first two arguments are not in the SQL values of the
  given list of tuples. This is commonly used with columns as you might in a @WHERE@ clause such as
  @WHERE (foo, bar) IN ((1,2), (3,4))@. Frequently you would not want to use this directly, but
  instead use 'Orville.PostgreSQL.Marshall.FieldDefinition.fieldTupleNotIn'.

@since 1.1.0.0
-}
tupleNotIn ::
  (SqlComparable a c, SqlComparable b d) =>
  a ->
  b ->
  NonEmpty (c, d) ->
  Expr.BooleanExpr
tupleNotIn a b =
  Expr.tupleNotIn
    (referenceValueExpression a :| [referenceValueExpression b])
    . fmap (toSqlValueTuple a b)
