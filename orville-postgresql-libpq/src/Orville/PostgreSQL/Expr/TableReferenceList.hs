{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.TableReferenceList
  ( TableReferenceList,
    referencesTable,
  )
where

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype TableReferenceList
  = TableReferenceList RawSql.RawSql
  deriving (RawSql.SqlExpression)

referencesTable :: Qualified TableName -> TableReferenceList
referencesTable qualifiedTableName =
  TableReferenceList $
    RawSql.toRawSql qualifiedTableName
