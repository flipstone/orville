{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Update
  ( UpdateExpr,
    updateExpr,
    SetClauseList,
    setClauseList,
    SetClause,
    setColumn,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

newtype UpdateExpr
  = UpdateExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

updateExpr ::
  Qualified TableName ->
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

setClauseList :: NonEmpty SetClause -> SetClauseList
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
