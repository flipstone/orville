{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.Qualified
  ( Qualified,
    qualified,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.SchemaName (SchemaName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a qualified SQL name.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
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

qualified ::
  IdentifierExpression name =>
  Maybe SchemaName ->
  name ->
  Qualified name
qualified mbSchemaName unqualifiedName =
  case mbSchemaName of
    Nothing ->
      Qualified . RawSql.toRawSql . toIdentifier $ unqualifiedName
    Just schemaName ->
      Qualified
        ( RawSql.toRawSql schemaName
            <> RawSql.dot
            <> RawSql.toRawSql (toIdentifier unqualifiedName)
        )
