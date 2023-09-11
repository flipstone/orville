{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2022-2023
License   : MIT
Stability: Stable

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
generated SQL.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The extension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) cursor name by hand and
use it in a place that expected a 'CursorName', that could be done as

 > RawSql.unsafeSqlExpression "my_cursor_name"

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
