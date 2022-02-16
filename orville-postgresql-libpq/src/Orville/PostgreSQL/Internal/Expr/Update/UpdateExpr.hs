{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Internal.Expr.Update.UpdateExpr
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Update.UpdateExpr
  ( UpdateExpr,
    updateExpr,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.Name (QualifiedTableName)
import Orville.PostgreSQL.Internal.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Internal.Expr.Update.SetClauseList (SetClauseList)
import Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype UpdateExpr
  = UpdateExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

updateExpr ::
  QualifiedTableName ->
  SetClauseList ->
  Maybe WhereClause ->
  Maybe ReturningExpr ->
  UpdateExpr
updateExpr tableName setClause maybeWhereClause maybeReturningExpr =
  UpdateExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "UPDATE"
        , Just $ RawSql.toRawSql tableName
        , Just $ RawSql.fromString "SET"
        , Just $ RawSql.toRawSql setClause
        , fmap RawSql.toRawSql maybeWhereClause
        , fmap RawSql.toRawSql maybeReturningExpr
        ]
