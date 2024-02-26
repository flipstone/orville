{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Alias
  ( Alias
  , stringToAlias
  , aliasToColumnName
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.ColumnName (ColumnName)
import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, fromIdentifier, identifier, toIdentifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL alias. 'Alias' values constructed
via the 'alias' function will be properly escaped as part of the
generated SQL. E.G.

> "some_alias"

'Alias' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype Alias
  = Alias Identifier
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    , -- | @since 1.1.0.0
      IdentifierExpression
    )

{- |
Construct an 'Alias' from a 'String' with proper escaping as part of the generated SQL.

@since 1.1.0.0
-}
stringToAlias :: String -> Alias
stringToAlias =
  Alias . identifier

{- |
It is occasionally appropriate to treat Construct an 'Alias' from a 'String' with proper escaping as part of the generated SQL.

@since 1.1.0.0
-}
aliasToColumnName :: Alias -> ColumnName
aliasToColumnName =
  fromIdentifier . toIdentifier
