{-|
Module    : Database.Orville.PostgreSQL.Expr.Where.BooleanExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Where.BooleanExpr
  ( BooleanExpr
  , booleanExprToSql
  , orExpr
  , andExpr
  , parenthesized
  , comparison
  , columnEquals
  , columnGreaterThan
  , columnLessThan
  , columnGreaterThanOrEqualTo
  , columnLessThanOrEqualTo
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.Name                     (ColumnName)
import           Database.Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator (ComparisonOperator, comparisonOperatorToSql, equalsOp, greaterThanOp, greaterThanOrEqualsOp, lessThanOp, lessThanOrEqualsOp)
import           Database.Orville.PostgreSQL.Internal.Expr.Where.RowValuePredicand  (RowValuePredicand, columnReference, comparisonValue, rowValuePredicandToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql                        as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue                      (SqlValue)

newtype BooleanExpr = BooleanExpr RawSql.RawSql

booleanExprToSql :: BooleanExpr -> RawSql.RawSql
booleanExprToSql (BooleanExpr sql) = sql

orExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
orExpr left right =
  BooleanExpr $
    booleanExprToSql left
    <> RawSql.fromString " OR "
    <> booleanExprToSql right

andExpr :: BooleanExpr -> BooleanExpr -> BooleanExpr
andExpr left right =
  BooleanExpr $
    booleanExprToSql left
    <> RawSql.fromString " AND "
    <> booleanExprToSql right

parenthesized :: BooleanExpr -> BooleanExpr
parenthesized expr =
  BooleanExpr $
    RawSql.fromString "(" <> booleanExprToSql expr <> RawSql.fromString ")"

comparison :: RowValuePredicand
           -> ComparisonOperator
           -> RowValuePredicand
           -> BooleanExpr
comparison left op right =
  BooleanExpr $
    rowValuePredicandToSql left
    <> RawSql.fromString " "
    <> comparisonOperatorToSql op
    <> RawSql.fromString " "
    <> rowValuePredicandToSql right

columnEquals :: ColumnName -> SqlValue -> BooleanExpr
columnEquals name value =
  comparison (columnReference name) equalsOp (comparisonValue value)

columnGreaterThan :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThan name value =
  comparison (columnReference name) greaterThanOp (comparisonValue value)

columnLessThan :: ColumnName -> SqlValue -> BooleanExpr
columnLessThan name value =
  comparison (columnReference name) lessThanOp (comparisonValue value)

columnGreaterThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThanOrEqualTo name value =
  comparison (columnReference name) greaterThanOrEqualsOp (comparisonValue value)

columnLessThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnLessThanOrEqualTo name value =
  comparison (columnReference name) lessThanOrEqualsOp (comparisonValue value)
