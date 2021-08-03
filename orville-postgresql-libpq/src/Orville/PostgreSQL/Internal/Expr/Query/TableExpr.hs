{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Query.TableExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Query.TableExpr
  ( TableExpr,
    tableExpr,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.GroupBy (GroupByClause)
import Orville.PostgreSQL.Internal.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Internal.Expr.Name (TableName)
import Orville.PostgreSQL.Internal.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Internal.Expr.OrderBy (OrderByClause)
import Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

tableExpr ::
  TableName ->
  Maybe WhereClause ->
  Maybe OrderByClause ->
  Maybe GroupByClause ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  TableExpr
tableExpr
  tableName
  maybeWhereClause
  maybeOrderByClause
  maybeGroupByClause
  maybeLimitExpr
  maybeOffsetExpr =
    TableExpr $
      RawSql.intercalate RawSql.space $
        (RawSql.toRawSql tableName) :
        catMaybes
          [ RawSql.toRawSql <$> maybeWhereClause
          , RawSql.toRawSql <$> maybeOrderByClause
          , RawSql.toRawSql <$> maybeGroupByClause
          , RawSql.toRawSql <$> maybeLimitExpr
          , RawSql.toRawSql <$> maybeOffsetExpr
          ]
