{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.WindowName
  ( WindowName
  , windowName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL window name. 'WindowName' values constructed
via the 'windowName' window will be properly escaped as part of the
generated SQL. E.G.

> "some_window_name"

'WindowName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype WindowName
  = WindowName Identifier
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    , -- | @since 1.1.0.0
      IdentifierExpression
    )

{- |
Construct a 'WindowName' from a 'String' with proper escaping as part of the generated SQL.

@since 1.1.0.0
-}
windowName :: String -> WindowName
windowName =
  WindowName . identifier
