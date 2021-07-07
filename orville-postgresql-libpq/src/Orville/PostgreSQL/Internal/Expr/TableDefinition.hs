module Orville.PostgreSQL.Internal.Expr.TableDefinition
  ( CreateTableExpr,
    createTableExpr,
    createTableExprToSql,
    PrimaryKeyExpr,
    primaryKeyExpr,
    primaryKeyToSql,
  )
where

import Data.List.NonEmpty (NonEmpty, toList)

import Orville.PostgreSQL.Internal.Expr.ColumnDefinition (ColumnDefinition, columnDefinitionToSql)
import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CreateTableExpr
  = CreateTableExpr RawSql.RawSql

createTableExpr ::
  TableName ->
  [ColumnDefinition] ->
  Maybe PrimaryKeyExpr ->
  CreateTableExpr
createTableExpr tableName columnDefs mbPrimaryKey =
  let columnDefsSql =
        map columnDefinitionToSql columnDefs

      tableElementsSql =
        case mbPrimaryKey of
          Nothing ->
            columnDefsSql
          Just primaryKey ->
            primaryKeyToSql primaryKey : columnDefsSql
   in CreateTableExpr $
        mconcat
          [ RawSql.fromString "CREATE TABLE "
          , RawSql.toRawSql tableName
          , RawSql.leftParen
          , RawSql.intercalate RawSql.comma tableElementsSql
          , RawSql.rightParen
          ]

createTableExprToSql :: CreateTableExpr -> RawSql.RawSql
createTableExprToSql (CreateTableExpr sql) = sql

newtype PrimaryKeyExpr
  = PrimaryKeyExpr RawSql.RawSql

primaryKeyToSql :: PrimaryKeyExpr -> RawSql.RawSql
primaryKeyToSql (PrimaryKeyExpr sql) = sql

primaryKeyExpr :: NonEmpty ColumnName -> PrimaryKeyExpr
primaryKeyExpr columnNames =
  PrimaryKeyExpr $
    mconcat
      [ RawSql.fromString "PRIMARY KEY "
      , RawSql.leftParen
      , RawSql.intercalate RawSql.comma (map RawSql.toRawSql (toList columnNames))
      , RawSql.rightParen
      ]
