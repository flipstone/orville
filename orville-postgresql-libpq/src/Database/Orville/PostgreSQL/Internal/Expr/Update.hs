{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Orville.PostgreSQL.Internal.Expr.Update
  ( UpdateExpr,
    updateExpr,
    SetClauseList,
    setClauseList,
    SetClause,
    setColumn,
  )
where

import Data.Maybe (maybeToList)

import Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName, TableName)
import Database.Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype UpdateExpr
  = UpdateExpr RawSql.RawSql
  deriving (RawSql.ToRawSql)

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
  deriving (RawSql.ToRawSql)

setClauseList :: [SetClause] -> SetClauseList
setClauseList setClauses =
  SetClauseList $
    RawSql.intercalate
      RawSql.comma
      (map RawSql.toRawSql setClauses)

newtype SetClause
  = SetClause RawSql.RawSql
  deriving (RawSql.ToRawSql)

setColumn :: ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value
