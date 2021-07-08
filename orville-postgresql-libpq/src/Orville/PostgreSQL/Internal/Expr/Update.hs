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

import Data.Maybe (maybeToList)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName)
import Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype UpdateExpr
  = UpdateExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

updateExpr ::
  TableName ->
  SetClauseList ->
  Maybe WhereClause ->
  UpdateExpr
updateExpr tableName setClause maybeWhereClause =
  UpdateExpr $
    RawSql.intercalate
      RawSql.space
      ( RawSql.fromString "UPDATE" :
        RawSql.toRawSql tableName :
        RawSql.fromString "SET" :
        RawSql.toRawSql setClause :
        maybeToList (RawSql.toRawSql <$> maybeWhereClause)
      )

newtype SetClauseList
  = SetClauseList RawSql.RawSql
  deriving (RawSql.SqlExpression)

setClauseList :: [SetClause] -> SetClauseList
setClauseList setClauses =
  SetClauseList $
    RawSql.intercalate
      RawSql.comma
      (map RawSql.toRawSql setClauses)

newtype SetClause
  = SetClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

setColumn :: ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value
