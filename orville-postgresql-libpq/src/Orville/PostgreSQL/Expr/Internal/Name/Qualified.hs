{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Qualified
  ( Qualified
  , qualifyTable
  , qualifySequence
  , qualifyColumn
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.ColumnName (ColumnName)
import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.SchemaName (SchemaName)
import Orville.PostgreSQL.Expr.Internal.Name.SequenceName (SequenceName)
import Orville.PostgreSQL.Expr.Internal.Name.TableName (TableName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a qualified SQL name.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The extension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) qualified name by hand
and use it in a place that expected a 'Qualified a', that could be done as

 > RawSql.unsafeSqlExpression "my.qualified_name"

@since 0.10.0.0
-}
newtype Qualified name
  = Qualified RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- | Optionally qualifies a 'TableName' with a 'SchemaName'. Generally you would want the higher
level function 'tableIdQualifiedName'.

@since 0.10.0.0
-}
qualifyTable ::
  Maybe SchemaName ->
  TableName ->
  Qualified TableName
qualifyTable = unsafeSchemaQualify

{- | Optionally qualifies a 'SequenceName' with a 'SchemaName'. Generally you would want the higher
level function 'sequenceIdQualifiedName'.

@since 0.10.0.0
-}
qualifySequence ::
  Maybe SchemaName ->
  SequenceName ->
  Qualified SequenceName
qualifySequence = unsafeSchemaQualify

{- | Qualifies a 'ColumnName' with a 'TableName' and, optionally, a 'SchemaName'. This should be
used to refer to the column in SQL queries where a qualified reference is appropriate.

@since 0.10.0.0
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
