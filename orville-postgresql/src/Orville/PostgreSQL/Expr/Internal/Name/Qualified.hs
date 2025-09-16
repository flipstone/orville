{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Qualified
  ( Qualified
  , qualifyTable
  , qualifySequence
  , qualifyFunction
  , qualifyIndex
  , qualifyConstraint
  , qualifyColumn
  , aliasQualifyColumn
  , QualifiedOrUnqualified
  , untrackQualified
  , unqualified
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Alias (AliasExpr)
import Orville.PostgreSQL.Expr.Internal.Name.ColumnName (ColumnName)
import Orville.PostgreSQL.Expr.Internal.Name.ConstraintName (ConstraintName)
import Orville.PostgreSQL.Expr.Internal.Name.FunctionName (FunctionName)
import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.IndexName (IndexName)
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
newtype Qualified name
  = Qualified RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

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

{- | Qualifies an 'IndexName' with a 'SchemaName'.
@since 1.1.0.0
-}
qualifyIndex ::
  SchemaName ->
  IndexName ->
  Qualified IndexName
qualifyIndex = unsafeSchemaQualify

{- | Qualifies a 'ConstraintName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.ConstraintIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.constraintIdQualifiedName' instead.
@since 1.2.0.0
-}
qualifyConstraint ::
  SchemaName ->
  ConstraintName ->
  Qualified ConstraintName
qualifyConstraint = unsafeSchemaQualify

{- | Qualifies a 'ColumnName' with a 'TableName' and, optionally, a 'SchemaName'.
This should be used to refer to the column in SQL queries where a qualified
reference is appropriate.

@since 1.0.0.0
-}
qualifyColumn :: Maybe SchemaName -> TableName -> ColumnName -> Qualified ColumnName
qualifyColumn mbSchemaName tableName unqualifiedName =
  let
    tableQualifiedColumn =
      rawQualify tableName unqualifiedName
  in
    case mbSchemaName of
      Nothing ->
        Qualified tableQualifiedColumn
      Just schemaName ->
        Qualified (rawQualify schemaName tableQualifiedColumn)

{- | Qualifies a 'ColumnName' with an 'AliasExpr'. This should be used to refer to the column
in SQL queries where an aliased reference is appropriate.

@since 1.1.0.0
-}
aliasQualifyColumn :: AliasExpr -> ColumnName -> Qualified ColumnName
aliasQualifyColumn aliasName =
  Qualified . rawQualify aliasName

-- Note: Not everything actually makes sense to be qualified by _only_ a schema name, such as
-- columns, as in 'qualifyColumn'. But this does give us a nice uniform way to provide the
-- functionality in those more type restricted scenarios.
unsafeSchemaQualify ::
  IdentifierExpression name =>
  SchemaName ->
  name ->
  Qualified name
unsafeSchemaQualify schemaName =
  Qualified . rawQualify schemaName . toIdentifier

{- |
  An internal helper to qualify any sql expressions as raw sql. The types for this
  function do not enforce any relationship between the qualifier and the qualified
  name, and the result is 'RawSql' rather than any more specific expression type.
-}
rawQualify ::
  (RawSql.SqlExpression qualifier, RawSql.SqlExpression name) =>
  qualifier ->
  name ->
  RawSql.RawSql
rawQualify qualifier name =
  RawSql.toRawSql qualifier
    <> RawSql.dot
    <> RawSql.toRawSql name
