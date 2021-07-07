{- |
Module    : Orville.PostgreSQL.Expr.Query.TableExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Query.TableExpr
  ( TableExpr,
    tableExpr,
    tableExprToSql,
  )
where

import Orville.PostgreSQL.Internal.Expr.GroupBy (GroupByClause, groupByClauseToSql)
import Orville.PostgreSQL.Internal.Expr.Name (TableName, tableNameToSql)
import Orville.PostgreSQL.Internal.Expr.OrderBy (OrderByClause, orderByClauseToSql)
import Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause, whereClauseToSql)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableExpr
  = TableExpr RawSql.RawSql

tableExprToSql :: TableExpr -> RawSql.RawSql
tableExprToSql (TableExpr sql) = sql

tableExpr ::
  TableName ->
  Maybe WhereClause ->
  Maybe OrderByClause ->
  Maybe GroupByClause ->
  TableExpr
tableExpr tableName mbWhereClause maybeOrderByClause maybeGroupByClause =
  TableExpr $
    tableNameToSql tableName
      <> RawSql.space
      <> maybe mempty whereClauseToSql mbWhereClause
      <> maybe mempty orderByClauseToSql maybeOrderByClause
      <> maybe mempty groupByClauseToSql maybeGroupByClause
