{-|
Module    : Database.Orville.PostgreSQL.Expr.Query.QueryExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Query.QueryExpr
  ( QueryExpr
  , queryExprToSql
  , queryExpr
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.Query.SelectList (SelectList, selectListToSql)
import           Database.Orville.PostgreSQL.Internal.Expr.Query.TableExpr  (TableExpr, tableExprToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql                as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr =
  QueryExpr RawSql.RawSql

queryExpr :: SelectList -> TableExpr -> QueryExpr
queryExpr selectList table =
  QueryExpr $
    mconcat
      [ RawSql.fromString "SELECT "
      , selectListToSql selectList
      , RawSql.fromString " FROM "
      , tableExprToSql table
      ]

queryExprToSql :: QueryExpr -> RawSql.RawSql
queryExprToSql (QueryExpr sql) = sql
