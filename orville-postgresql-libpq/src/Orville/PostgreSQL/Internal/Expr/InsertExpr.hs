{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.InsertExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.InsertExpr
  ( InsertExpr,
    insertExpr,
    InsertColumnList,
    insertColumnList,
    InsertSource,
    insertSqlValues,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, QualifiedTableName)
import Orville.PostgreSQL.Internal.Expr.ReturningExpr (ReturningExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertExpr ::
  QualifiedTableName ->
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
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen

newtype InsertSource
  = InsertSource RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma rows

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
