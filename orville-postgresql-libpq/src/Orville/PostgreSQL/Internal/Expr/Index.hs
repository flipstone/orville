{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Index
  ( CreateIndexExpr,
    createIndexExpr,
    IndexUniqueness (UniqueIndex, NonUniqueIndex),
    DropIndexExpr,
    dropIndexExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, IndexName, Qualified, TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CreateIndexExpr
  = CreateIndexExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

createIndexExpr ::
  IndexUniqueness ->
  Qualified TableName ->
  NonEmpty ColumnName ->
  CreateIndexExpr
createIndexExpr uniqueness tableName columns =
  CreateIndexExpr $
    RawSql.fromString "CREATE "
      <> uniquenessToSql uniqueness
      <> RawSql.fromString "INDEX ON "
      <> RawSql.toRawSql tableName
      <> RawSql.space
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columns
      <> RawSql.rightParen

data IndexUniqueness
  = UniqueIndex
  | NonUniqueIndex
  deriving (Eq, Ord, Show)

uniquenessToSql :: IndexUniqueness -> RawSql.RawSql
uniquenessToSql uniqueness =
  case uniqueness of
    UniqueIndex -> RawSql.fromString "UNIQUE "
    NonUniqueIndex -> mempty

newtype DropIndexExpr
  = DropIndexExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

dropIndexExpr :: IndexName -> DropIndexExpr
dropIndexExpr indexName =
  DropIndexExpr $
    RawSql.fromString "DROP INDEX " <> RawSql.toRawSql indexName
