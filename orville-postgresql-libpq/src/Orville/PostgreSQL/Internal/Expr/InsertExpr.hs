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

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertExpr :: TableName -> Maybe InsertColumnList -> InsertSource -> InsertExpr
insertExpr target _ source =
  InsertExpr $
    mconcat
      [ RawSql.fromString "INSERT INTO "
      , RawSql.toRawSql target
      , RawSql.space
      , RawSql.toRawSql source
      ]

newtype InsertColumnList
  = InsertColumnList RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertColumnList :: [ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.intercalate RawSql.comma (map RawSql.toRawSql columnNames)

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
