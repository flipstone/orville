{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.ColumnName
  ( ColumnName
  , columnName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL column name. 'ColumnName' values constructed via the
'columnName' function will be properly escaped as part of the generated SQL. E.G.

> "some_column_name"

'ColumnName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype ColumnName
  = ColumnName Identifier
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
columnName :: String -> ColumnName
columnName = ColumnName . identifier
