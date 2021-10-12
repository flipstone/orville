{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Update
  ( UpdateExpr,
    updateExpr,
    SetClauseList,
    setClauseList,
    SetClause,
    setColumn,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, QualifiedTableName)
import Orville.PostgreSQL.Internal.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

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

newtype SetClauseList
  = SetClauseList RawSql.RawSql
  deriving (RawSql.SqlExpression)

setClauseList :: [SetClause] -> SetClauseList
setClauseList =
  SetClauseList . RawSql.intercalate RawSql.comma

newtype SetClause
  = SetClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

setColumn :: ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value
