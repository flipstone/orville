{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Qualified
  ( Qualified
  , qualifyTable
  , qualifySequence
  , qualifyFunction
  , qualifyColumn
  , aliasQualifyColumn
  , QualifiedOrUnqualified
  , untrackQualified
  , unqualified
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Alias (AliasExpr)
import Orville.PostgreSQL.Expr.Internal.Name.ColumnName (ColumnName)
import Orville.PostgreSQL.Expr.Internal.Name.FunctionName (FunctionName)
import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.SchemaName (SchemaName)
import Orville.PostgreSQL.Expr.Internal.Name.SequenceName (SequenceName)
import Orville.PostgreSQL.Expr.Internal.Name.TableName (TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL name that could be qualified or not. E.G.

> "some_schema_name"."some_table_name"

or

> "some_table_name"

'QualifiedOrUnqualified' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype QualifiedOrUnqualified name
  = QualifiedOrUnqualified RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | 'untrackQualified' loses the information of the value certainly having a qualification. This is
   useful when working with functionality that takes a 'QualifiedOrUnqualifed name'.

@since 1.1.0.0
-}
untrackQualified :: Qualified name -> QualifiedOrUnqualified name
untrackQualified =
  QualifiedOrUnqualified . RawSql.toRawSql

{- | 'unqualified' loses the information of the value not having a qualification. This is
   useful when working with functionality that takes a 'QualifiedOrUnqualifed name'.

@since 1.1.0.0
-}
unqualified :: RawSql.SqlExpression name => name -> QualifiedOrUnqualified name
unqualified =
  QualifiedOrUnqualified . RawSql.toRawSql

{- | Type to represent a qualified SQL name. E.G.

> "some_schema_name"."some_table_name"

'Qualified' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
data Qualified name = Qualified
  { i_qualifier :: RawSql.RawSql
  , i_itemQualified :: RawSql.RawSql
  }

-- | @since 1.0.0.0
instance RawSql.SqlExpression (Qualified name) where
  toRawSql (Qualified q i) =
    q <> RawSql.dot <> i
  unsafeFromRawSql = Qualified mempty

{- | Qualifies a 'TableName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.TableIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.tableIdQualifiedName' instead.
@since 1.0.0.0
-}
qualifyTable ::
  SchemaName ->
  TableName ->
  Qualified TableName
qualifyTable = unsafeSchemaQualify

{- | Qualifies a 'SequenceName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.SequenceIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.sequenceIdQualifiedName' instead.

@since 1.0.0.0
-}
qualifySequence ::
  SchemaName ->
  SequenceName ->
  Qualified SequenceName
qualifySequence = unsafeSchemaQualify

{- | Qualifies a 'FunctionName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.FunctionIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.functionIdQualifiedName' instead.

@since 1.1.0.0
-}
qualifyFunction ::
  SchemaName ->
  FunctionName ->
  Qualified FunctionName
qualifyFunction = unsafeSchemaQualify

{- | Qualifies a 'ColumnName' with a 'TableName' and, optionally, a 'SchemaName'.
This should be used to refer to the column in SQL queries where a qualified
reference is appropriate.

@since 1.0.0.0
-}
qualifyColumn :: Maybe SchemaName -> TableName -> ColumnName -> Qualified ColumnName
qualifyColumn mbSchemaName tableName unqualifiedName =
  case mbSchemaName of
    Nothing ->
      Qualified
        { i_qualifier = RawSql.toRawSql (toIdentifier tableName)
        , i_itemQualified = RawSql.toRawSql (toIdentifier unqualifiedName)
        }
    Just schemaName ->
      Qualified
        { i_qualifier = (RawSql.toRawSql schemaName) <> RawSql.dot <> RawSql.toRawSql tableName
        , i_itemQualified = RawSql.toRawSql (toIdentifier unqualifiedName)
        }

{- | Qualifies a 'ColumnName' with an 'AliasExpr'. This should be used to refer to the column
in SQL queries where an aliased reference is appropriate.

@since 1.1.0.0
-}
aliasQualifyColumn :: AliasExpr -> ColumnName -> Qualified ColumnName
aliasQualifyColumn aliasName =
  Qualified (RawSql.toRawSql $ toIdentifier aliasName) . RawSql.toRawSql . toIdentifier

-- Note: Not everything actually makes sense to be qualified by _only_ a schema name, such as
-- columns, as in 'qualifyColumn'. But this does give us a nice uniform way to provide the
-- functionality in those more type restricted scenarios.
unsafeSchemaQualify ::
  IdentifierExpression name =>
  SchemaName ->
  name ->
  Qualified name
unsafeSchemaQualify schemaName =
  Qualified (RawSql.toRawSql schemaName) . RawSql.toRawSql . toIdentifier
