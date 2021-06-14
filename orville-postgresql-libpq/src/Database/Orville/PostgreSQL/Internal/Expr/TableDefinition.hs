module Database.Orville.PostgreSQL.Internal.Expr.TableDefinition
  ( CreateTableExpr,
    createTableExpr,
    createTableExprToSql,
    PrimaryKeyExpr,
    primaryKeyExpr,
    primaryKeyToSql,
  )
where

import Database.Orville.PostgreSQL.Internal.Expr.ColumnDefinition (ColumnDefinition, columnDefinitionToSql)
import Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName, columnNameToSql, tableNameToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

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
          , tableNameToSql tableName
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

primaryKeyExpr :: [ColumnName] -> PrimaryKeyExpr
primaryKeyExpr columnNames =
  PrimaryKeyExpr $
    mconcat
      [ RawSql.fromString "PRIMARY KEY "
      , RawSql.leftParen
      , RawSql.intercalate RawSql.comma (map columnNameToSql columnNames)
      , RawSql.rightParen
      ]
