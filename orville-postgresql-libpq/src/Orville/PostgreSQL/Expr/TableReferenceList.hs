{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.TableReferenceList
  ( TableReferenceList
  , referencesTable
  , aliasReferencesTable
  )
where

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype TableReferenceList
  = TableReferenceList RawSql.RawSql
  deriving (RawSql.SqlExpression)

referencesTable :: Qualified TableName -> TableReferenceList
referencesTable =
  TableReferenceList . RawSql.toRawSql

aliasReferencesTable :: Qualified TableName -> TableName -> TableReferenceList
aliasReferencesTable qualifiedTableName asTableName =
  TableReferenceList
    (RawSql.toRawSql qualifiedTableName <> RawSql.fromString " AS " <> RawSql.toRawSql asTableName)
