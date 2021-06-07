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

import           Database.Orville.PostgreSQL.Internal.Expr.Name (TableName, tableNameToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql    as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue  (SqlValue)

newtype InsertExpr =
  InsertExpr RawSql.RawSql

insertExprToSql :: InsertExpr -> RawSql.RawSql
insertExprToSql (InsertExpr sql) = sql

insertExpr :: TableName -> InsertSource -> InsertExpr
insertExpr target source =
  InsertExpr $
    mconcat
      [ RawSql.fromString "INSERT INTO "
      , tableNameToSql target
      , RawSql.fromString " "
      , insertSourceToSql source
      ]

newtype InsertSource =
  InsertSource RawSql.RawSql

insertSourceToSql :: InsertSource -> RawSql.RawSql
insertSourceToSql (InsertSource sql) = sql

insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
    <> RawSql.intercalate (RawSql.fromString ",") (fmap rowValuesToSql rows)

insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues rows =
  insertRowValues (fmap rowValues rows)

newtype RowValues =
  RowValues RawSql.RawSql

rowValuesToSql :: RowValues -> RawSql.RawSql
rowValuesToSql (RowValues sql) = sql

rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.fromString "("
      , RawSql.intercalate (RawSql.fromString ",") (fmap RawSql.parameter values)
      , RawSql.fromString ")"
      ]
