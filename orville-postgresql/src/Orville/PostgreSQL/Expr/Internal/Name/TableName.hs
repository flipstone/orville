{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.TableName
  ( TableName
  , tableName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL table name. 'TableName' values constructed via the
'tableName' function will be properly escaped as part of the generated SQL.
E.G.

> "some_table_name"

'TableName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype TableName
  = TableName Identifier
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    , -- | @since 1.0.0.0
      IdentifierExpression
    )

{- | Construct a 'TableName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
tableName :: String -> TableName
tableName =
  TableName . identifier
