{-|
Module    : Database.Orville.PostgreSQL.Expr.Query.TableExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Query.TableExpr
  ( TableExpr
  , tableExpr
  , tableExprToSql
  ) where

import Database.Orville.PostgreSQL.Internal.Expr.Name              (TableName, tableNameToSql)
import Database.Orville.PostgreSQL.Internal.Expr.OrderBy           (OrderByClause, orderByClauseToSql)
import Database.Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause, whereClauseToSql)
import Database.Orville.PostgreSQL.Internal.RawSql                 (RawSql, fromString)

newtype TableExpr =
  TableExpr RawSql

tableExprToSql :: TableExpr -> RawSql
tableExprToSql (TableExpr sql) = sql

tableExpr :: TableName -> Maybe WhereClause -> Maybe OrderByClause -> TableExpr
tableExpr tableName mbWhereClause maybeOrderByClause =
  TableExpr $
    tableNameToSql tableName
    <> fromString " "
    <> maybe mempty whereClauseToSql mbWhereClause
    <> maybe mempty orderByClauseToSql maybeOrderByClause
