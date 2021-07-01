{- |
Module    : Database.Orville.PostgreSQL.Expr.Query.TableExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Expr.Query.TableExpr
  ( TableExpr,
    tableExpr,
    tableExprToSql,
  )
where

import Database.Orville.PostgreSQL.Internal.Expr.GroupBy (GroupByClause, groupByClauseToSql)
import Database.Orville.PostgreSQL.Internal.Expr.Name (TableName, tableNameToSql)
import Database.Orville.PostgreSQL.Internal.Expr.OrderBy (OrderByClause, orderByClauseToSql)
import Database.Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause, whereClauseToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

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
