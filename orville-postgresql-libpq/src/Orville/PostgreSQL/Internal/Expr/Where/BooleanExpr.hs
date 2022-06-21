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
    columnEquals,
    columnNotEquals,
    columnGreaterThan,
    columnLessThan,
    columnGreaterThanOrEqualTo,
    columnLessThanOrEqualTo,
    columnIsNull,
    columnIsNotNull,
    comparison,
    columnIn,
    columnNotIn,
    columnTupleIn,
    columnTupleNotIn,
    InValuePredicate,
    inPredicate,
    notInPredicate,
    inValueList,
  )
where

import qualified Data.List.NonEmpty as NE
import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import Orville.PostgreSQL.Internal.Expr.ValueExpression (ValueExpression, columnReference, rowValueConstructor, valueExpression)
import Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator (ComparisonOperator, equalsOp, greaterThanOp, greaterThanOrEqualsOp, lessThanOp, lessThanOrEqualsOp, notEqualsOp)
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
  inPredicate (columnReference columnName) (inValueList $ fmap valueExpression values)

columnNotIn :: ColumnName -> NE.NonEmpty SqlValue -> BooleanExpr
columnNotIn columnName values =
  notInPredicate (columnReference columnName) (inValueList $ fmap valueExpression values)

{- |
  Checks that the tuple constructed from the given columns in one of the tuples
  specified in the input list. It is up to the caller to ensure that all the
  tuples given have the same arity.
-}
columnTupleIn :: NE.NonEmpty ColumnName -> NE.NonEmpty (NE.NonEmpty SqlValue) -> BooleanExpr
columnTupleIn columnNames valueLists =
  inPredicate
    (rowValueConstructor $ fmap columnReference columnNames)
    (inValueList $ fmap (rowValueConstructor . fmap valueExpression) valueLists)

{- |
  Checks that the tuple constructed from the given columns is NOT one of the
  tuples specified in the input list. It is up to the caller to ensure that all
  the tuples given have the same arity.
-}
columnTupleNotIn :: NE.NonEmpty ColumnName -> NE.NonEmpty (NE.NonEmpty SqlValue) -> BooleanExpr
columnTupleNotIn columnNames valueLists =
  notInPredicate
    (rowValueConstructor $ fmap columnReference columnNames)
    (inValueList $ fmap (rowValueConstructor . fmap valueExpression) valueLists)

inPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
inPredicate predicand predicate =
  BooleanExpr $
    RawSql.toRawSql predicand
      <> RawSql.fromString " IN "
      <> RawSql.toRawSql predicate

notInPredicate :: ValueExpression -> InValuePredicate -> BooleanExpr
notInPredicate predicand predicate =
  BooleanExpr $
    RawSql.toRawSql predicand
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

comparison ::
  ValueExpression ->
  ComparisonOperator ->
  ValueExpression ->
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
  comparison (columnReference name) equalsOp (valueExpression value)

columnNotEquals :: ColumnName -> SqlValue -> BooleanExpr
columnNotEquals name value =
  comparison (columnReference name) notEqualsOp (valueExpression value)

columnGreaterThan :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThan name value =
  comparison (columnReference name) greaterThanOp (valueExpression value)

columnLessThan :: ColumnName -> SqlValue -> BooleanExpr
columnLessThan name value =
  comparison (columnReference name) lessThanOp (valueExpression value)

columnGreaterThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnGreaterThanOrEqualTo name value =
  comparison (columnReference name) greaterThanOrEqualsOp (valueExpression value)

columnLessThanOrEqualTo :: ColumnName -> SqlValue -> BooleanExpr
columnLessThanOrEqualTo name value =
  comparison (columnReference name) lessThanOrEqualsOp (valueExpression value)

columnIsNull :: ColumnName -> BooleanExpr
columnIsNull name =
  BooleanExpr $
    RawSql.toRawSql (columnReference name)
      <> RawSql.space
      <> RawSql.fromString "IS NULL"

columnIsNotNull :: ColumnName -> BooleanExpr
columnIsNotNull name =
  BooleanExpr $
    RawSql.toRawSql (columnReference name)
      <> RawSql.space
      <> RawSql.fromString "IS NOT NULL"
