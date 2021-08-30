{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Query.QueryExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Query.QueryExpr
  ( QueryExpr,
    queryExpr,
    SelectClause,
    selectClause,
    SelectExpr,
    selectExpr,
    Distinct (..),
  )
where

import qualified Data.Maybe as Maybe

import Orville.PostgreSQL.Internal.Expr.Query.SelectList (SelectList)
import Orville.PostgreSQL.Internal.Expr.Query.TableExpr (TableExpr)
import Orville.PostgreSQL.Internal.Expr.Select (Distinct (Distinct), SelectClause, SelectExpr, selectClause, selectExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

queryExpr :: SelectClause -> SelectList -> Maybe TableExpr -> QueryExpr
queryExpr querySelectClause selectList maybeTableExpr =
  let maybeFromClause = do
        tableExpr <- maybeTableExpr
        pure (RawSql.fromString " FROM " <> RawSql.toRawSql tableExpr)
   in QueryExpr $
        mconcat
          [ RawSql.toRawSql querySelectClause
          , RawSql.toRawSql selectList
          , Maybe.fromMaybe (RawSql.fromString "") maybeFromClause
          ]
