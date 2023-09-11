{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.SchemaName
  ( SchemaName
  , schemaName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL schema name. 'SchemaName' values constructed via the
'schemaName' function will be properly escaped as part of the generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The extension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) schema name by hand and
use it in a place that expected a 'SchemaName', that could be done as

 > RawSql.unsafeSqlExpression "my_schema_name"

@since 0.10.0.0
-}
newtype SchemaName
  = SchemaName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'ColumnName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
schemaName :: String -> SchemaName
schemaName =
  SchemaName . identifier
