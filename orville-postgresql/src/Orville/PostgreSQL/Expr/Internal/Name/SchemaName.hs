{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
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
E.G.

> "some_schema_name"

'SchemaName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SchemaName
  = SchemaName Identifier
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    , -- | @since 1.0.0.0
      IdentifierExpression
    )

{- |
Construct a 'ColumnName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
schemaName :: String -> SchemaName
schemaName =
  SchemaName . identifier
