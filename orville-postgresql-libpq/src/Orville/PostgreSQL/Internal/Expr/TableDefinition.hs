{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.TableDefinition
  ( CreateTableExpr,
    createTableExpr,
    PrimaryKeyExpr,
    primaryKeyExpr,
  )
where

import Data.List.NonEmpty (NonEmpty, toList)

import Orville.PostgreSQL.Internal.Expr.ColumnDefinition (ColumnDefinition)
import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CreateTableExpr
  = CreateTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

createTableExpr ::
  TableName ->
  [ColumnDefinition] ->
  Maybe PrimaryKeyExpr ->
  CreateTableExpr
createTableExpr tableName columnDefs mbPrimaryKey =
  let columnDefsSql =
        map RawSql.toRawSql columnDefs

      tableElementsSql =
        case mbPrimaryKey of
          Nothing ->
            columnDefsSql
          Just primaryKey ->
            RawSql.toRawSql primaryKey : columnDefsSql
   in CreateTableExpr $
        mconcat
          [ RawSql.fromString "CREATE TABLE "
          , RawSql.toRawSql tableName
          , RawSql.leftParen
          , RawSql.intercalate RawSql.comma tableElementsSql
          , RawSql.rightParen
          ]

newtype PrimaryKeyExpr
  = PrimaryKeyExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

primaryKeyExpr :: NonEmpty ColumnName -> PrimaryKeyExpr
primaryKeyExpr columnNames =
  PrimaryKeyExpr $
    mconcat
      [ RawSql.fromString "PRIMARY KEY "
      , RawSql.leftParen
      , RawSql.intercalate RawSql.comma (map RawSql.toRawSql (toList columnNames))
      , RawSql.rightParen
      ]
