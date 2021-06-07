{-|
Module    : Database.Orville.PostgreSQL.Expr.InsertExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.InsertExpr
  ( InsertExpr
  , insertExpr
  , insertExprToSql
  , InsertSource
  , insertSqlValues
  ) where

import Database.Orville.PostgreSQL.Internal.Expr.Name (TableName, tableNameToSql)
import Database.Orville.PostgreSQL.Internal.RawSql    (RawSql, fromString, intercalate, parameter)
import Database.Orville.PostgreSQL.Internal.SqlValue  (SqlValue)

newtype InsertExpr =
  InsertExpr RawSql

insertExprToSql :: InsertExpr -> RawSql
insertExprToSql (InsertExpr sql) = sql

insertExpr :: TableName -> InsertSource -> InsertExpr
insertExpr target source =
  InsertExpr $
    mconcat
      [ fromString "INSERT INTO "
      , tableNameToSql target
      , fromString " "
      , insertSourceToSql source
      ]

newtype InsertSource =
  InsertSource RawSql

insertSourceToSql :: InsertSource -> RawSql
insertSourceToSql (InsertSource sql) = sql

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    fromString "VALUES "
    <> intercalate (fromString ",") (fmap rowValuesToSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (fmap rowValues rows)

newtype RowValues =
  RowValues RawSql

rowValuesToSql :: RowValues -> RawSql
rowValuesToSql (RowValues sql) = sql

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ fromString "("
      , intercalate (fromString ",") (fmap parameter values)
      , fromString ")"
      ]
