{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.TableConstraint
  ( TableConstraint,
    uniqueConstraint,
    foreignKeyConstraint,
    ForeignKeyActionExpr,
    restrictExpr,
    cascadeExpr,
    setNullExpr,
    setDefaultExpr,
    ForeignKeyDeleteActionExpr,
    foreignKeyDeleteActionExpr,
    ForeignKeyUpdateActionExpr,
    foreignKeyUpdateActionExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, Qualified, TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableConstraint
  = TableConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

uniqueConstraint :: NonEmpty ColumnName -> TableConstraint
uniqueConstraint columnNames =
  TableConstraint $
    RawSql.fromString "UNIQUE "
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen

newtype ForeignKeyActionExpr
  = ForeignKeyActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

restrictExpr :: ForeignKeyActionExpr
restrictExpr = ForeignKeyActionExpr $ RawSql.fromString "RESTRICT"

cascadeExpr :: ForeignKeyActionExpr
cascadeExpr = ForeignKeyActionExpr $ RawSql.fromString "CASCADE"

setNullExpr :: ForeignKeyActionExpr
setNullExpr = ForeignKeyActionExpr $ RawSql.fromString "SET NULL"

setDefaultExpr :: ForeignKeyActionExpr
setDefaultExpr = ForeignKeyActionExpr $ RawSql.fromString "SET DEFAULT"

newtype ForeignKeyUpdateActionExpr
  = ForeignKeyUpdateActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

foreignKeyUpdateActionExpr :: ForeignKeyActionExpr -> ForeignKeyUpdateActionExpr
foreignKeyUpdateActionExpr action =
  ForeignKeyUpdateActionExpr $
    RawSql.fromString "ON UPDATE"
      <> RawSql.space
      <> RawSql.toRawSql action

newtype ForeignKeyDeleteActionExpr
  = ForeignKeyDeleteActionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

foreignKeyDeleteActionExpr :: ForeignKeyActionExpr -> ForeignKeyDeleteActionExpr
foreignKeyDeleteActionExpr action =
  ForeignKeyDeleteActionExpr $
    RawSql.fromString "ON DELETE"
      <> RawSql.space
      <> RawSql.toRawSql action

foreignKeyConstraint ::
  NonEmpty ColumnName ->
  Qualified TableName ->
  NonEmpty ColumnName ->
  Maybe ForeignKeyUpdateActionExpr ->
  Maybe ForeignKeyDeleteActionExpr ->
  TableConstraint
foreignKeyConstraint columnNames foreignTableName foreignColumnNames mbUpdateAction mbDeleteAction =
  TableConstraint $
    RawSql.fromString "FOREIGN KEY "
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen
      <> RawSql.fromString " REFERENCES "
      <> RawSql.toRawSql foreignTableName
      <> RawSql.space
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma foreignColumnNames
      <> RawSql.rightParen
      <> maybe mempty (\updateAction -> RawSql.space <> RawSql.toRawSql updateAction) mbUpdateAction
      <> maybe mempty (\deleteAction -> RawSql.space <> RawSql.toRawSql deleteAction) mbDeleteAction
