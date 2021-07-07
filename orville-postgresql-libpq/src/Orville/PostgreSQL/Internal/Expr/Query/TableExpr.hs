{- |
Module    : Orville.PostgreSQL.Expr.Query.TableExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Query.TableExpr
  ( TableExpr,
    tableExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.GroupBy (GroupByClause)
import Orville.PostgreSQL.Internal.Expr.Name (TableName)
import Orville.PostgreSQL.Internal.Expr.OrderBy (OrderByClause)
import Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving RawSql.SqlExpression

tableExpr ::
  TableName ->
  Maybe WhereClause ->
  Maybe OrderByClause ->
  Maybe GroupByClause ->
  TableExpr
tableExpr tableName mbWhereClause maybeOrderByClause maybeGroupByClause =
  TableExpr $
    RawSql.toRawSql tableName
      <> RawSql.space
      <> maybe mempty RawSql.toRawSql mbWhereClause
      <> maybe mempty RawSql.toRawSql maybeOrderByClause
      <> maybe mempty RawSql.toRawSql maybeGroupByClause
