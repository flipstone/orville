{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.SelectOptions.WhereCondition
  ( WhereCondition,
    fieldEquals,
    fieldNotEquals,
    fieldGreaterThan,
    fieldLessThan,
    fieldGreaterThanOrEqualTo,
    fieldLessThanOrEqualTo,
    fieldIn,
    fieldNotIn,
    fieldTupleIn,
    fieldTupleNotIn,
    whereAnd,
    whereOr,
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
  Checks that the value in a field does not equal a particular value.
-}
fieldNotEquals :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldNotEquals =
  whereColumnComparison Expr.columnNotEquals

{- |
  Checks that the value in a field is greater than a particular value.
-}
fieldGreaterThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldGreaterThan =
  whereColumnComparison Expr.columnGreaterThan

{- |
  Checks that the value in a field is less than a particular value.
-}
fieldLessThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldLessThan =
  whereColumnComparison Expr.columnLessThan

{- |
  Checks that the value in a field is greater than or equal to a particular value.
-}
fieldGreaterThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldGreaterThanOrEqualTo =
  whereColumnComparison Expr.columnGreaterThanOrEqualTo

{- |
  Checks that the value in a field is less than or equal to a particular value.
-}
fieldLessThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
fieldLessThanOrEqualTo =
  whereColumnComparison Expr.columnLessThanOrEqualTo

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
  Checks that a field does not match a list of values
-}
fieldNotIn :: FieldDef.FieldDefinition nullability a -> NonEmpty a -> WhereCondition
fieldNotIn fieldDef values =
  WhereCondition $
    Expr.columnNotIn
      (FieldDef.fieldColumnName fieldDef)
      (fmap (FieldDef.fieldValueToSqlValue fieldDef) values)

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
whereAnd :: NonEmpty WhereCondition -> WhereCondition
whereAnd =
  foldParenthenizedExprs Expr.andExpr

{- |
  Combines multiple 'WhereCondition's together using 'OR.
-}
whereOr :: NonEmpty WhereCondition -> WhereCondition
whereOr =
  foldParenthenizedExprs Expr.orExpr

{- |
  INTERNAL: Combines a (non-empty) list of 'WhereCondition's together using
  the provided function to combine each pair.
-}
foldParenthenizedExprs ::
  (Expr.BooleanExpr -> Expr.BooleanExpr -> Expr.BooleanExpr) ->
  NonEmpty WhereCondition ->
  WhereCondition
foldParenthenizedExprs foldExpr conditions =
  let (first :| rest) =
        fmap
          (Expr.parenthesized . whereConditionToBooleanExpr)
          conditions
   in WhereCondition $ foldr (flip foldExpr) first rest
