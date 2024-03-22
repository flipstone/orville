{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.ExtensionName
  ( ExtensionName
  , extensionName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a PostgreSQL extension name. 'ExtensionName' values constructed via the
'extensionName' function will be properly escaped as part of the generated SQL. E.G.

> "some_extension_name"

'ExtensionName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype ExtensionName
  = ExtensionName Identifier
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    , -- | @since 1.1.0.0
      IdentifierExpression
    )

{- |
Construct an 'ExtensionName' from a 'String'.

@since 1.1.0.0
-}
extensionName :: String -> ExtensionName
extensionName = ExtensionName . identifier
