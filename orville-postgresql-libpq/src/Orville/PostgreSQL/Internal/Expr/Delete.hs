{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Delete
  ( DeleteExpr,
    deleteExpr,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.Name (QualifiedTableName)
import Orville.PostgreSQL.Internal.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype DeleteExpr
  = DeleteExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

deleteExpr ::
  QualifiedTableName ->
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
