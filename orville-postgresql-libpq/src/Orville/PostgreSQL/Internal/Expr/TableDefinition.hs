{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.TableDefinition
  ( CreateTableExpr,
    createTableExpr,
    PrimaryKeyExpr,
    primaryKeyExpr,
    AlterTableExpr,
    alterTableExpr,
    AlterTableAction,
    addColumn,
    DropTableExpr,
    dropTableExpr,
    IfExists,
    ifExists,
  )
where

import Data.Maybe (catMaybes)
import Data.List.NonEmpty (NonEmpty, toList)

import Orville.PostgreSQL.Internal.Expr.ColumnDefinition (ColumnDefinition)
import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, QualifiedTableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype CreateTableExpr
  = CreateTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

createTableExpr ::
  QualifiedTableName ->
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
          , RawSql.space
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

newtype AlterTableExpr
  = AlterTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

alterTableExpr :: QualifiedTableName -> NonEmpty AlterTableAction -> AlterTableExpr
alterTableExpr tableName actions =
  AlterTableExpr $
    RawSql.fromString "ALTER TABLE "
    <> RawSql.toRawSql tableName
    <> RawSql.space
    <> RawSql.intercalate RawSql.comma (map RawSql.toRawSql (toList actions))

newtype AlterTableAction
  = AlterTableAction RawSql.RawSql
  deriving (RawSql.SqlExpression)

addColumn :: ColumnDefinition -> AlterTableAction
addColumn columnDef =
  AlterTableAction $
    RawSql.fromString "ADD COLUMN " <> RawSql.toRawSql columnDef


newtype DropTableExpr
  = DropTableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

dropTableExpr :: Maybe IfExists -> QualifiedTableName -> DropTableExpr
dropTableExpr maybeIfExists tableName =
  DropTableExpr $
    RawSql.intercalate
      RawSql.space
      (catMaybes
        [ Just (RawSql.fromString "DROP TABLE")
        , fmap RawSql.toRawSql maybeIfExists
        , Just (RawSql.toRawSql tableName)
        ]
      )

newtype IfExists
  = IfExists RawSql.RawSql
  deriving (RawSql.SqlExpression)

ifExists :: IfExists
ifExists =
  IfExists $ RawSql.fromString "IF EXISTS"
