{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.TableReferenceList
  ( TableReferenceList
  , referencesTable
  , referencesTableWithAlias
  )
where

import Orville.PostgreSQL.Expr.Name (AliasExpr, Qualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the table's references in the @FROM@ clause of a @SELECT
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

{- |
  Constructs a 'TableReferenceList' consisting of the specified table AS the given alias.

  @since 1.1.0.0
-}
referencesTableWithAlias :: AliasExpr -> Qualified TableName -> TableReferenceList
referencesTableWithAlias alias qualifiedTableName =
  TableReferenceList $
    RawSql.intercalate
      RawSql.space
      [ RawSql.toRawSql qualifiedTableName
      , RawSql.fromString "AS"
      , RawSql.toRawSql alias
      ]
