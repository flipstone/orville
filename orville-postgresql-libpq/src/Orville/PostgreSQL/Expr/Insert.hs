{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Insert
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
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

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

newtype InsertColumnList
  = InsertColumnList RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertColumnList :: [ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)
      <> RawSql.rightParen

newtype InsertSource
  = InsertSource RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (fmap rowValues rows)

newtype RowValues
  = RowValues RawSql.RawSql
  deriving (RawSql.SqlExpression)

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.leftParen
      , RawSql.intercalate RawSql.comma (fmap RawSql.parameter values)
      , RawSql.rightParen
      ]
