{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Orville.PostgreSQL.Internal.SelectOptions.WhereCondition
  ( WhereCondition,
    whereEquals,
    whereNotEquals,
    whereGreaterThan,
    whereLessThan,
    whereGreaterThanOrEqualTo,
    whereLessThanOrEqualTo,
    whereAnd,
    whereOr,
    whereBooleanExpr,
    whereConditionToBooleanExpr,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A 'WhereCondition' represents a SQL expression that can be added to SELECT
  statements to restrict the rows returned by a query. Various functions
  are provided below to construct conditions.
-}
newtype WhereCondition
  = WhereCondition Expr.BooleanExpr
  deriving (RawSql.ToRawSql)

{- |
  Constructs a 'WhereCondition' from a 'Expr.BooleanExpr'. You can use this
  to together with the 'RawSql.RawSql' related functions in the 'Expr' module
  to use SQL expressions Orville does not have direct support for in your
  queries.
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
whereEquals :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereEquals =
  whereColumnComparison Expr.columnEquals

{- |
  Checks that the value in a field does not equal a particular value.
-}
whereNotEquals :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereNotEquals =
  whereColumnComparison Expr.columnNotEquals

{- |
  Checks that the value in a field is greater than a particular value.
-}
whereGreaterThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereGreaterThan =
  whereColumnComparison Expr.columnGreaterThan

{- |
  Checks that the value in a field is less than a particular value.
-}
whereLessThan :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereLessThan =
  whereColumnComparison Expr.columnLessThan

{- |
  Checks that the value in a field is greater than or equal to a particular value.
-}
whereGreaterThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereGreaterThanOrEqualTo =
  whereColumnComparison Expr.columnGreaterThanOrEqualTo

{- |
  Checks that the value in a field is less than or equal to a particular value.
-}
whereLessThanOrEqualTo :: FieldDef.FieldDefinition nullability a -> a -> WhereCondition
whereLessThanOrEqualTo =
  whereColumnComparison Expr.columnLessThanOrEqualTo

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
