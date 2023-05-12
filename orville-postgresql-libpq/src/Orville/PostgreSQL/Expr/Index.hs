{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Index
  ( CreateIndexExpr,
    createIndexExpr,
    IndexUniqueness (UniqueIndex, NonUniqueIndex),
    DropIndexExpr,
    dropIndexExpr,
    createNamedIndexExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (ColumnName, IndexName, Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "CREATE INDEX" statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a create index by hand and use it in a place that
expected a 'CreateIndexExpr', that could be done as

 > RawSql.unsafeSqlExpression "CREATE INDEX <some unusual index creation>"

@since 0.10.0.0
-}
newtype CreateIndexExpr
  = CreateIndexExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

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

{- |
Type to represent a SQL "DROP INDEX" statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a drop index by hand and use it in a place that
expected a 'DropIndexExpr', that could be done as

 > RawSql.unsafeSqlExpression "DROP INDEX <some unusual index drop>"

@since 0.10.0.0
-}
newtype DropIndexExpr
  = DropIndexExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

dropIndexExpr :: IndexName -> DropIndexExpr
dropIndexExpr indexName =
  DropIndexExpr $
    RawSql.fromString "DROP INDEX " <> RawSql.toRawSql indexName

createNamedIndexExpr ::
  IndexUniqueness ->
  Qualified TableName ->
  IndexName ->
  RawSql.RawSql ->
  CreateIndexExpr
createNamedIndexExpr uniqueness tableName indexName indexSql =
  CreateIndexExpr $
    RawSql.fromString "CREATE "
      <> uniquenessToSql uniqueness
      <> RawSql.fromString "INDEX "
      <> RawSql.toRawSql indexName
      <> RawSql.fromString " ON "
      <> RawSql.toRawSql tableName
      <> RawSql.space
      <> indexSql
