{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Alias
  ( AliasExpr
  , stringToAliasExpr
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a SQL alias. 'AliasExpr' values constructed
via the 'alias' function will be properly escaped as part of the
generated SQL. E.G.

> "some_alias"

'AliasExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype AliasExpr
  = AliasExpr Identifier
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    , -- | @since 1.1.0.0
      IdentifierExpression
    )

{- | Construct an 'AliasExpr' from a 'String' with proper escaping as part of the generated SQL.

@since 1.1.0.0
-}
stringToAliasExpr :: String -> AliasExpr
stringToAliasExpr =
  AliasExpr . identifier
