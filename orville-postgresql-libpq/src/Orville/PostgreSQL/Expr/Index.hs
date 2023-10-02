{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability: Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Index
  ( CreateIndexExpr
  , createIndexExpr
  , IndexUniqueness (UniqueIndex, NonUniqueIndex)
  , IndexBodyExpr
  , indexBodyColumns
  , ConcurrentlyExpr
  , concurrently
  , DropIndexExpr
  , dropIndexExpr
  , createNamedIndexExpr
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Name (ColumnName, IndexName, Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL "CREATE INDEX" statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The extension mechanism provided does require
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

{- |
Construct a SQL CREATE INDEX from an indicator of if the index should be
unique, a table, and corresponding collection of 'ColumnName's.

@since 0.10.0.0
-}
createIndexExpr ::
  IndexUniqueness ->
  Maybe ConcurrentlyExpr ->
  Qualified TableName ->
  NonEmpty ColumnName ->
  CreateIndexExpr
createIndexExpr uniqueness mbConcurrently tableName columns =
  CreateIndexExpr $
    RawSql.fromString "CREATE "
      <> uniquenessToSql uniqueness
      <> RawSql.fromString "INDEX "
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbConcurrently)
      <> RawSql.fromString "ON "
      <> RawSql.toRawSql tableName
      <> RawSql.space
      <> RawSql.toRawSql (indexBodyColumns columns)

{- |
Construct a SQL CREATE INDEX from an indicator of if the index should be
unique, a table, a name for the index, and some sql representing the rest of
the index creation.

@since 0.10.0.0
-}
createNamedIndexExpr ::
  IndexUniqueness ->
  Maybe ConcurrentlyExpr ->
  Qualified TableName ->
  IndexName ->
  IndexBodyExpr ->
  CreateIndexExpr
createNamedIndexExpr uniqueness mbConcurrently tableName indexName bodyExpr =
  CreateIndexExpr $
    RawSql.fromString "CREATE "
      <> uniquenessToSql uniqueness
      <> RawSql.fromString "INDEX "
      <> maybe mempty (<> RawSql.space) (fmap RawSql.toRawSql mbConcurrently)
      <> RawSql.toRawSql indexName
      <> RawSql.fromString " ON "
      <> RawSql.toRawSql tableName
      <> RawSql.space
      <> RawSql.toRawSql bodyExpr

{- |
Type to represent the @CONCURRENTLY@ keyword for index creation

'ConcurrentlyExpr' provides a 'RawSql.SqlExpression' instance. See
'unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 0.10.0.0
-}
newtype ConcurrentlyExpr
  = ConcurrentlyExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- |
The @CONCURRENTLY@ keyword indicates to PostgreSQL that an index should be
create concurrently.

@since 0.10.0.0
-}
concurrently :: ConcurrentlyExpr
concurrently =
  RawSql.unsafeSqlExpression "CONCURRENTLY"

{- |
Type to represent if the body of an index definition (i.e. @<body>@ in
after @CREATE some_index ON some_table <body>@).

'IndexBodyExpr' provides a 'RawSql.SqlExpression' instance. See
'unsafeSqlExpression' for how to construct a value with your own custom SQL.

@since 0.10.0.0
-}
newtype IndexBodyExpr
  = IndexBodyExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- |
Creates an 'IndexBodyExpr' for the given column names. The resulting
SQL looks like @(column1, column2, ...)@.

@since 0.10.0.0
-}
indexBodyColumns ::
  NonEmpty ColumnName ->
  IndexBodyExpr
indexBodyColumns columns =
  IndexBodyExpr $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columns
      <> RawSql.rightParen

{- |
Type to represent if an index should be unique.

@since 0.10.0.0
-}
data IndexUniqueness
  = UniqueIndex
  | NonUniqueIndex
  deriving
    ( -- | @since 0.10.0.0
      Eq
    , -- | @since 0.10.0.0
      Ord
    , -- | @since 0.10.0.0
      Show
    )

-- Internal helper
uniquenessToSql :: IndexUniqueness -> RawSql.RawSql
uniquenessToSql uniqueness =
  case uniqueness of
    UniqueIndex -> RawSql.fromString "UNIQUE "
    NonUniqueIndex -> mempty

{- |
Type to represent a SQL "DROP INDEX" statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The extension mechanism provided does require
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

{- |
Construct a SQL DROP INDEX for a given 'IndexName'.

@since 0.10.0.0
-}
dropIndexExpr :: IndexName -> DropIndexExpr
dropIndexExpr indexName =
  DropIndexExpr $
    RawSql.fromString "DROP INDEX " <> RawSql.toRawSql indexName
