{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.TableConstraint
  ( TableConstraint,
    uniqueConstraint,
    foreignKeyConstraint,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, QualifiedTableName)
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

foreignKeyConstraint ::
  NonEmpty ColumnName ->
  QualifiedTableName ->
  NonEmpty ColumnName ->
  TableConstraint
foreignKeyConstraint columnNames foreignTableName foreignColumnNames =
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
