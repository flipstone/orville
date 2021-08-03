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
    comparison,
    columnEquals,
    columnNotEquals,
    columnGreaterThan,
    columnLessThan,
    columnGreaterThanOrEqualTo,
    columnLessThanOrEqualTo,
    columnIn,
    columnNotIn,
  )
where

import qualified Data.List.NonEmpty as NE
import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator (ComparisonOperator, equalsOp, greaterThanOp, greaterThanOrEqualsOp, lessThanOp, lessThanOrEqualsOp, notEqualsOp)
import Orville.PostgreSQL.Internal.Expr.Where.RowValuePredicand (RowValuePredicand, columnReference, comparisonValue)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

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

columnIn :: ColumnName -> NE.NonEmpty SqlValue -> BooleanExpr
columnIn columnName values =
  BooleanExpr $
    RawSql.toRawSql columnName
      <> RawSql.fromString " IN "
      <> RawSql.leftParen
      <> RawSql.intercalate (RawSql.comma <> RawSql.space) (map (RawSql.toRawSql . RawSql.parameter) $ NE.toList values)
      <> RawSql.rightParen

columnNotIn :: ColumnName -> NE.NonEmpty SqlValue -> BooleanExpr
columnNotIn columnName values =
  BooleanExpr $
    RawSql.toRawSql columnName
      <> RawSql.fromString " NOT IN "
      <> RawSql.leftParen
      <> RawSql.intercalate (RawSql.comma <> RawSql.space) (map (RawSql.toRawSql . RawSql.parameter) $ NE.toList values)
      <> RawSql.rightParen

parenthesized :: BooleanExpr -> BooleanExpr
parenthesized expr =
  BooleanExpr $
    RawSql.leftParen <> RawSql.toRawSql expr <> RawSql.rightParen

comparison ::
  RowValuePredicand ->
  ComparisonOperator ->
  RowValuePredicand ->
  BooleanExpr
comparison left op right =
  BooleanExpr $
    RawSql.toRawSql left
      <> RawSql.space
      <> RawSql.toRawSql op
      <> RawSql.space
      <> RawSql.toRawSql right

columnEquals :: ColumnName -> SqlValue -> BooleanExpr
columnEquals name value =
  comparison (columnReference name) equalsOp (comparisonValue value)

columnNotEquals :: ColumnName -> SqlValue -> BooleanExpr
columnNotEquals name value =
  comparison (columnReference name) notEqualsOp (comparisonValue value)

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
