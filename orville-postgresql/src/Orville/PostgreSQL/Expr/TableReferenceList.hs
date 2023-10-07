{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.TableReferenceList
  ( TableReferenceList
  , referencesTable
  )
where

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the tables references in the @FROM@ clause of a @SELECT
statement. E.G. just the

> foo

in

> FROM foo

'TableReferenceList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype TableReferenceList
  = TableReferenceList RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'TableReferenceList' consisting of just the specified table
  name.

  @since 1.0.0.0
-}
referencesTable :: Qualified TableName -> TableReferenceList
referencesTable qualifiedTableName =
  TableReferenceList $
    RawSql.toRawSql qualifiedTableName
