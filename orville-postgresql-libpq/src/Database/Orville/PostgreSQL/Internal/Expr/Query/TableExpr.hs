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

import Data.Maybe (catMaybes)

import Database.Orville.PostgreSQL.Internal.Expr.LimitExpr (LimitExpr)
import Database.Orville.PostgreSQL.Internal.Expr.Name (TableName)
import Database.Orville.PostgreSQL.Internal.Expr.OrderBy (OrderByClause)
import Database.Orville.PostgreSQL.Internal.Expr.Where.WhereClause (WhereClause)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableExpr
  = TableExpr RawSql.RawSql

tableExprToSql :: TableExpr -> RawSql.RawSql
tableExprToSql (TableExpr sql) = sql

tableExpr ::
  TableName ->
  Maybe WhereClause ->
  Maybe OrderByClause ->
  Maybe LimitExpr ->
  TableExpr
tableExpr tableName mbWhereClause mbOrderByClause mbLimitExpr =
  TableExpr $
    RawSql.intercalate RawSql.space $
      (RawSql.toRawSql tableName) :
      catMaybes
        [ RawSql.toRawSql <$> mbWhereClause
        , RawSql.toRawSql <$> mbOrderByClause
        , RawSql.toRawSql <$> mbLimitExpr
        ]
