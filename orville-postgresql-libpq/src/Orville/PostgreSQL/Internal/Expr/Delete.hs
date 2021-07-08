{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Delete
  ( DeleteExpr,
    deleteExpr,
  )
where

import Data.Maybe (maybeToList)

import Orville.PostgreSQL.Internal.Expr.Name (TableName)
import Orville.PostgreSQL.Internal.Expr.Where (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype DeleteExpr
  = DeleteExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

deleteExpr ::
  TableName ->
  Maybe WhereClause ->
  DeleteExpr
deleteExpr tableName maybeWhereClause =
  DeleteExpr $
    RawSql.intercalate
      RawSql.space
      ( RawSql.fromString "DELETE FROM" :
        RawSql.toRawSql tableName :
        maybeToList (RawSql.toRawSql <$> maybeWhereClause)
      )
