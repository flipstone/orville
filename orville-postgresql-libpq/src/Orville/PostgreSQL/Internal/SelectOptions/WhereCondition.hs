{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.SelectOptions.WhereCondition
  ( WhereCondition,
    fieldEquals,
    (.==),
    fieldNotEquals,
    (./=),
    fieldGreaterThan,
    (.>),
    fieldLessThan,
    (.<),
    fieldGreaterThanOrEqualTo,
    (.>=),
    fieldLessThanOrEqualTo,
    (.<=),
    fieldIsNull,
    fieldIsNotNull,
    fieldIn,
    (.<-),
    fieldNotIn,
    (.</-),
    fieldTupleIn,
    fieldTupleNotIn,
    whereAnd,
    (.&&),
    whereOr,
    (.||),
    whereBooleanExpr,
    whereConditionToBooleanExpr,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A 'WhereCondition' represents a SQL expression that can be added to SELECT
  statements to restrict the rows returned by a query. Various functions
  are provided below to construct conditions.
-}
newtype WhereCondition
  = WhereCondition Expr.BooleanExpr
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'WhereCondition' from a 'Expr.BooleanExpr'. You can use this to
  together with the 'RawSql.RawSql' related functions in the 'Expr' module to
  use SQL expressions that Orville does not directly support in your queries.
-}
whereBooleanExpr :: Expr.BooleanExpr -> WhereCondition
whereBooleanExpr =
  WhereCondition

{- |
  Converts a 'WhereCondition' into a 'Expr.BooleanExpr' so that you can use it
  with the functions from the 'Expr' module if you are building SQL expressions
  yourself.
-}
whereConditionToBooleanExpr :: WhereCondition -> Expr.BooleanExpr
whereConditionToBooleanExpr (WhereCondition expr) =
  expr

{- |
  Checks that the value in a field equals a particular value.
-}
fieldEquals :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldEquals =
  whereColumnComparison Expr.columnEquals

{- |
  Operator alias for 'fieldEquals'
-}
(.==) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(.==) = fieldEquals

infixl 9 .==

{- |
  Checks that the value in a field does not equal a particular value.
-}
fieldNotEquals :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldNotEquals =
  whereColumnComparison Expr.columnNotEquals

{- |
  Operator alias for 'fieldNotEquals'
-}
(./=) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(./=) = fieldNotEquals

infixl 9 ./=

{- |
  Checks that the value in a field is greater than a particular value.
-}
fieldGreaterThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldGreaterThan =
  whereColumnComparison Expr.columnGreaterThan

{- |
  Operator alias for 'fieldGreaterThan'
-}
(.>) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(.>) = fieldGreaterThan

infixl 9 .>

{- |
  Checks that the value in a field is less than a particular value.
-}
fieldLessThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldLessThan =
  whereColumnComparison Expr.columnLessThan

{- |
  Operator alias for 'fieldLessThan'
-}
(.<) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(.<) = fieldLessThan

infixl 9 .<

{- |
  Checks that the value in a field is greater than or equal to a particular value.
-}
fieldGreaterThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldGreaterThanOrEqualTo =
  whereColumnComparison Expr.columnGreaterThanOrEqualTo

{- |
  Operator alias for 'fieldGreaterThanOrEqualTo'
-}
(.>=) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(.>=) = fieldGreaterThanOrEqualTo

infixl 9 .>=

{- |
  Checks that the value in a field is less than or equal to a particular value.
-}
fieldLessThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldLessThanOrEqualTo =
  whereColumnComparison Expr.columnLessThanOrEqualTo

{- |
  Operator alias for 'fieldLessThanOrEqualTo'
-}
(.<=) :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
(.<=) = fieldLessThanOrEqualTo

infixl 9 .<=

{- |
  Checks that the value in a field is null.
-}
fieldIsNull :: FieldDef.FieldDefinition FieldDef.Nullable a -> WhereCondition
fieldIsNull =
  WhereCondition . Expr.columnIsNull . FieldDef.fieldColumnName

{- |
  Checks that the value in a field is not null.
-}
fieldIsNotNull :: FieldDef.FieldDefinition FieldDef.Nullable a -> WhereCondition
fieldIsNotNull =
  WhereCondition . Expr.columnIsNotNull . FieldDef.fieldColumnName

{- |
  Checks that a field matches a list of values
-}
fieldIn :: FieldDef.FieldDefinition nullability a -> NonEmpty a -> WhereCondition
fieldIn fieldDef values =
  WhereCondition $
    Expr.columnIn
      (FieldDef.fieldColumnName fieldDef)
      (fmap (FieldDef.fieldValueToSqlValue fieldDef) values)

{- |
  Operator alias for 'fieldIn'
-}
(.<-) :: FieldDef.FieldDefinition nullability a -> NonEmpty a -> WhereCondition
(.<-) = fieldIn

infixl 9 .<-

{- |
  Checks that a field does not match a list of values
-}
fieldNotIn :: FieldDef.FieldDefinition nullability a -> NonEmpty a -> WhereCondition
fieldNotIn fieldDef values =
  WhereCondition $
    Expr.columnNotIn
      (FieldDef.fieldColumnName fieldDef)
      (fmap (FieldDef.fieldValueToSqlValue fieldDef) values)

{- |
  Operator alias for 'fieldNotIn'
-}
(.</-) :: FieldDef.FieldDefinition nullability a -> NonEmpty a -> WhereCondition
(.</-) = fieldNotIn

infixl 9 .</-

{- |
  Checks that a tuple of two fields is in the list of specified tuplies
-}
fieldTupleIn ::
  FieldDef.FieldDefinition nullabilityA a ->
  FieldDef.FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  WhereCondition
fieldTupleIn fieldDefA fieldDefB values =
  WhereCondition $
    Expr.columnTupleIn
      (FieldDef.fieldColumnName fieldDefA :| [FieldDef.fieldColumnName fieldDefB])
      (fmap (toSqlValueTuple fieldDefA fieldDefB) values)

{- |
  Checks that a tuple of two fields is not in the list of specified tuplies
-}
fieldTupleNotIn ::
  FieldDef.FieldDefinition nullabilityA a ->
  FieldDef.FieldDefinition nullabilityB b ->
  NonEmpty (a, b) ->
  WhereCondition
fieldTupleNotIn fieldDefA fieldDefB values =
  WhereCondition $
    Expr.columnTupleNotIn
      (FieldDef.fieldColumnName fieldDefA :| [FieldDef.fieldColumnName fieldDefB])
      (fmap (toSqlValueTuple fieldDefA fieldDefB) values)

{- |
  Constructs a SqlValue "tuple" (i.e. NonEmpty list) for two fields
-}
toSqlValueTuple ::
  FieldDef.FieldDefinition nullabilityA a ->
  FieldDef.FieldDefinition nullabilityB b ->
  (a, b) ->
  NonEmpty SqlValue.SqlValue
toSqlValueTuple fieldDefA fieldDefB (a, b) =
  FieldDef.fieldValueToSqlValue fieldDefA a
    :| [FieldDef.fieldValueToSqlValue fieldDefB b]

{- |
  INTERNAL: Constructs a field-based 'WhereCondition' using a function that
  builds a 'Expr.BooleanExpr'
-}
whereColumnComparison ::
  (Expr.ColumnName -> SqlValue.SqlValue -> Expr.BooleanExpr) ->
  (FieldDef.FieldDefinition nullability a -> a -> WhereCondition)
whereColumnComparison columnComparison fieldDef a =
  WhereCondition $
    columnComparison
      (FieldDef.fieldColumnName fieldDef)
      (FieldDef.fieldValueToSqlValue fieldDef a)

{- |
  Combines multiple 'WhereCondition's together using 'AND'.
-}
whereAnd :: WhereCondition -> WhereCondition -> WhereCondition
whereAnd left right =
  whereBooleanExpr $
    Expr.andExpr
      (Expr.parenthesized (whereConditionToBooleanExpr left))
      (Expr.parenthesized (whereConditionToBooleanExpr right))

{- |
  Operator alias for 'whereAnd'
-}
(.&&) :: WhereCondition -> WhereCondition -> WhereCondition
(.&&) = whereAnd

infixr 8 .&&

{- |
  Combines multiple 'WhereCondition's together using 'OR.
-}
whereOr :: WhereCondition -> WhereCondition -> WhereCondition
whereOr left right =
  whereBooleanExpr $
    Expr.orExpr
      (Expr.parenthesized (whereConditionToBooleanExpr left))
      (Expr.parenthesized (whereConditionToBooleanExpr right))

{- |
  Operator alias for 'whereOr
-}
(.||) :: WhereCondition -> WhereCondition -> WhereCondition
(.||) = whereOr

infixr 8 .||
