{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Insert
  ( InsertExpr,
    insertExpr,
    InsertColumnList,
    insertColumnList,
    InsertSource,
    insertSqlValues,
    RowValues,
    rowValues,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Raw.SqlValue (SqlValue)

{- |
Type to represent a SQL "INSERT" statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write an insert by hand and use it in a place that
expected a 'InsertExpr', that could be done as

 > RawSql.unsafeSqlExpression "INSERT <some unusual insert>"

@since 0.10.0.0
-}
newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

insertExpr ::
  Qualified TableName ->
  Maybe InsertColumnList ->
  InsertSource ->
  Maybe ReturningExpr ->
  InsertExpr
insertExpr target maybeInsertColumns source maybeReturning =
  InsertExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "INSERT INTO"
        , Just $ RawSql.toRawSql target
        , fmap RawSql.toRawSql maybeInsertColumns
        , Just $ RawSql.toRawSql source
        , fmap RawSql.toRawSql maybeReturning
        ]

{- |
Type to represent the SQL columns list for an insert statement

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write insert columns by hand and use them in a
place that expected a 'InsertExpr', that could be done as

 > RawSql.unsafeSqlExpression "(my,raw,column,list)"

@since 0.10.0.0
-}
newtype InsertColumnList
  = InsertColumnList RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

insertColumnList :: [ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)
      <> RawSql.rightParen

{- |
Type to represent the SQL for the source of data for an insert statement (e.g.
a @VALUES@ clause).

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write an insert source by hand and use
it in a place that expected a 'InsertSource', that could be done as

 > RawSql.unsafeSqlExpression "VALUES ('my','raw','insert','values')"

@since 0.10.0.0
-}
newtype InsertSource
  = InsertSource RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (fmap rowValues rows)

{- |
Type to represent a SQL row literal (e.g. for use as a row to insert
inside a @VALUES@ clause).

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a row by hand and use it in a place that
expected a 'RowValues', that could be done as

 > RawSql.unsafeSqlExpression "('my','raw','insert','values')"

@since 0.10.0.0
-}
newtype RowValues
  = RowValues RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.leftParen
      , RawSql.intercalate RawSql.comma (fmap RawSql.parameter values)
      , RawSql.rightParen
      ]
