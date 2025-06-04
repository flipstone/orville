{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.TableReferenceList
  ( TableReferenceList
  , tableReferenceList
  , singleTableReferenceList
  , TableReference
  , tableNameReference
  )
where

import Orville.PostgreSQL.Expr.Name (AliasExpr, QualifiedOrUnqualified, TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the table references in the @FROM@ clause of a @SELECT
statement. E.G. just the

> foo, bar

in

> FROM foo, bar

'TableReferenceList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype TableReferenceList
  = TableReferenceList RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'TableReferenceList' from a list of 'TableReference' values.
  The table references will be separated by commas in the resulting list

@since 1.1.0.0
-}
tableReferenceList :: [TableReference] -> TableReferenceList
tableReferenceList =
  TableReferenceList . RawSql.intercalate RawSql.commaSpace

{- |
  A convenience function for constructing a 'TableReferenceList' that references
  just a single table with no alias.

  See also 'tableReferenceList' and 'tableNameReference'.

@since 1.1.0.0
-}
singleTableReferenceList :: QualifiedOrUnqualified TableName -> TableReferenceList
singleTableReferenceList name =
  tableReferenceList [tableNameReference name Nothing]

{- |
Type to represent a single table references in the @FROM@ clause of a @SELECT
statement. E.G. just the

> foo

in

> FROM foo, bar

'TableReferenceList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype TableReference
  = TableReference RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'TableReference' for the specified table name.
  name.

  @since 1.1.0.0
-}
tableNameReference ::
  QualifiedOrUnqualified TableName ->
  Maybe AliasExpr ->
  TableReference
tableNameReference qualifiedTableName mbAliasExpr =
  TableReference $
    RawSql.toRawSql qualifiedTableName
      <> case mbAliasExpr of
        Nothing -> mempty
        Just aliasExpr -> RawSql.fromString " AS " <> RawSql.toRawSql aliasExpr
