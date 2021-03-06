{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Query.QueryExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Query.QueryExpr
  ( QueryExpr,
    queryExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.Query.SelectList (SelectList)
import Orville.PostgreSQL.Internal.Expr.Query.TableExpr (TableExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

queryExpr :: SelectList -> TableExpr -> QueryExpr
queryExpr selectList table =
  QueryExpr $
    mconcat
      [ RawSql.fromString "SELECT "
      , RawSql.toRawSql selectList
      , RawSql.fromString " FROM "
      , RawSql.toRawSql table
      ]
