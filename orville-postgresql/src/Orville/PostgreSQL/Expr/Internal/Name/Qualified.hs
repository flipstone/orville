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
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.ColumnName (ColumnName)
import Orville.PostgreSQL.Expr.Internal.Name.FunctionName (FunctionName)
import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.SchemaName (SchemaName)
import Orville.PostgreSQL.Expr.Internal.Name.SequenceName (SequenceName)
import Orville.PostgreSQL.Expr.Internal.Name.TableName (TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a qualified SQL name. E.G.

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

{- |
Optionally qualifies a 'TableName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.TableIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.tableIdQualifiedName' instead.
@since 1.0.0.0
-}
qualifyTable ::
  Maybe SchemaName ->
  TableName ->
  Qualified TableName
qualifyTable = unsafeSchemaQualify

{- |
Optionally qualifies a 'SequenceName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.SequenceIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.sequenceIdQualifiedName' instead.

@since 1.0.0.0
-}
qualifySequence ::
  Maybe SchemaName ->
  SequenceName ->
  Qualified SequenceName
qualifySequence = unsafeSchemaQualify

{- |
Optionally qualifies a 'FunctionName' with a 'SchemaName'.

Note: If you already have a 'Orville.PostgreSQL.Schema.FunctionIdentifier' in
hand you should probably use
'Orville.PostgreSQL.Schema.functionIdQualifiedName' instead.

@since 1.1.0.0
-}
qualifyFunction ::
  Maybe SchemaName ->
  FunctionName ->
  Qualified FunctionName
qualifyFunction = unsafeSchemaQualify

{- |
Qualifies a 'ColumnName' with a 'TableName' and, optionally, a 'SchemaName'.
This should be used to refer to the column in SQL queries where a qualified
reference is appropriate.

@since 1.0.0.0
-}
qualifyColumn :: Maybe SchemaName -> TableName -> ColumnName -> Qualified ColumnName
qualifyColumn mbSchemaName tableName unqualifiedName =
  unsafeSchemaQualify mbSchemaName
    . RawSql.unsafeFromRawSql
    $ RawSql.toRawSql (toIdentifier tableName) <> RawSql.dot <> RawSql.toRawSql (toIdentifier unqualifiedName)

-- Note: Not everything actually makes sense to be qualified by _only_ a schema name, such as
-- columns, as in 'qualifyColumn'. But this does give us a nice uniform way to provide the
-- functionality in those more type restricted scenarios.
unsafeSchemaQualify ::
  IdentifierExpression name =>
  Maybe SchemaName ->
  name ->
  Qualified name
unsafeSchemaQualify mbSchemaName unqualifiedName =
  case mbSchemaName of
    Nothing ->
      Qualified . RawSql.toRawSql . toIdentifier $ unqualifiedName
    Just schemaName ->
      Qualified
        ( RawSql.toRawSql schemaName
            <> RawSql.dot
            <> RawSql.toRawSql (toIdentifier unqualifiedName)
        )
