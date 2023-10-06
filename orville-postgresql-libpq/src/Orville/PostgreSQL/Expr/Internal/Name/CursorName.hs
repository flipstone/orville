{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.CursorName
  ( CursorName
  , cursorName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL cursor name. 'CursorName' values constructed
via the 'cursorName' function will be properly escaped as part of the
generated SQL. E.G.

> "some_cursor_name"

'CursorName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype CursorName
  = CursorName Identifier
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    , -- | @since 0.10.0.0
      IdentifierExpression
    )

{- |
Construct a 'CursorName' from a 'String' with proper escaping as part of the generated SQL.

@since 0.10.0.0
-}
cursorName :: String -> CursorName
cursorName =
  CursorName . identifier
