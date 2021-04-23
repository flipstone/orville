{-|
Module    : Database.Orville.PostgreSQL.Expr
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr
  ( QueryExpr
  , queryExpr
  , SelectList
  , selectStar
  , TableExpr
  , tableExpr
  , TableName
  , rawTableName
  , InsertExpr
  , insertExpr
  , queryExprToSql
  , insertExprToSql
  ) where

import qualified Data.ByteString.Char8 as B8

import           Database.Orville.PostgreSQL.Internal.RawSql (RawSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr =
  QueryExpr RawSql

queryExpr :: SelectList -> TableExpr -> QueryExpr
queryExpr selectList table =
  QueryExpr $
    mconcat
      [ RawSql.fromString "SELECT "
      , selectListToSql selectList
      , RawSql.fromString " FROM "
      , tableExprToSql table
      ]

queryExprToSql :: QueryExpr -> RawSql
queryExprToSql (QueryExpr sql) = sql

newtype SelectList =
  SelectList RawSql

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectListToSql :: SelectList -> RawSql
selectListToSql (SelectList sql) =
  sql

newtype TableExpr =
  TableExpr RawSql

tableExprToSql :: TableExpr -> RawSql
tableExprToSql (TableExpr sql) = sql

tableExpr :: TableName -> TableExpr
tableExpr = TableExpr . tableNameToSql

newtype TableName =
  TableName RawSql

tableNameToSql :: TableName -> RawSql
tableNameToSql (TableName sql) = sql

rawTableName :: String -> TableName
rawTableName =
  TableName . RawSql.fromString

newtype InsertExpr =
  InsertExpr RawSql

insertExpr :: TableName -> [B8.ByteString] -> InsertExpr
insertExpr target rowValues =
  InsertExpr $
    mconcat
      [ RawSql.fromString "INSERT INTO "
      , tableNameToSql target
      , RawSql.fromString " VALUES ("
      , RawSql.intercalate (RawSql.fromString ",") (map RawSql.parameter rowValues)
      , RawSql.fromString ")"
    ]

insertExprToSql :: InsertExpr -> RawSql
insertExprToSql (InsertExpr sql) = sql
