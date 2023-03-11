{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Delete
  ( DeleteExpr,
    deleteExpr,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype DeleteExpr
  = DeleteExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

deleteExpr ::
  Qualified TableName ->
  Maybe WhereClause ->
  Maybe ReturningExpr ->
  DeleteExpr
deleteExpr tableName maybeWhereClause maybeReturningExpr =
  DeleteExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "DELETE FROM"
        , Just $ RawSql.toRawSql tableName
        , fmap RawSql.toRawSql maybeWhereClause
        , fmap RawSql.toRawSql maybeReturningExpr
        ]
