{- |
Module    : Database.Orville.PostgreSQL.Expr.InsertExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Expr.InsertExpr
  ( InsertExpr,
    insertExpr,
    insertExprToSql,
    InsertColumnList,
    insertColumnList,
    insertColumnListToSql,
    InsertSource,
    insertSqlValues,
  )
where

import Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName, columnNameToSql, tableNameToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype InsertExpr
  = InsertExpr RawSql.RawSql

insertExprToSql :: InsertExpr -> RawSql.RawSql
insertExprToSql (InsertExpr sql) = sql

insertExpr :: TableName -> Maybe InsertColumnList -> InsertSource -> InsertExpr
insertExpr target _ source =
  InsertExpr $
    mconcat
      [ RawSql.fromString "INSERT INTO "
      , tableNameToSql target
      , RawSql.space
      , insertSourceToSql source
      ]

newtype InsertColumnList
  = InsertColumnList RawSql.RawSql

insertColumnListToSql :: InsertColumnList -> RawSql.RawSql
insertColumnListToSql (InsertColumnList sql) = sql

insertColumnList :: [ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.intercalate (RawSql.comma) (map columnNameToSql columnNames)

newtype InsertSource
  = InsertSource RawSql.RawSql

insertSourceToSql :: InsertSource -> RawSql.RawSql
insertSourceToSql (InsertSource sql) = sql

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma (fmap rowValuesToSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (fmap rowValues rows)

newtype RowValues
  = RowValues RawSql.RawSql

rowValuesToSql :: RowValues -> RawSql.RawSql
rowValuesToSql (RowValues sql) = sql

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.leftParen
      , RawSql.intercalate RawSql.comma (fmap RawSql.parameter values)
      , RawSql.rightParen
      ]
