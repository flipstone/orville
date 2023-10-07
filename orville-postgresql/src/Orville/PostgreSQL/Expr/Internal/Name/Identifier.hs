{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Internal.Name.Identifier
  ( Identifier
  , identifier
  , identifierFromBytes
  , IdentifierExpression (toIdentifier, fromIdentifier)
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL identifier. 'Identifier' values constructed via the
'identifier' function will be properly escaped as part of the generated SQL.
E.G.

> "some_identifier"

'Identifer' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype Identifier
  = Identifier RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
Construct an 'Identifier' from a 'String' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
identifier :: String -> Identifier
identifier =
  identifierFromBytes . B8.pack

{- |
Construct an 'Identifier' from a 'B8.ByteString' with proper escaping as part of the generated SQL.

@since 1.0.0.0
-}
identifierFromBytes :: B8.ByteString -> Identifier
identifierFromBytes =
  Identifier . RawSql.identifier

{- | This class aids in giving additional polymorphism and so that many different identifiers can be
created without being forced to only use the `Identifier` type.

@since 1.0.0.0
-}
class IdentifierExpression name where
  -- | @since 1.0.0.0
  toIdentifier :: name -> Identifier

  -- | @since 1.0.0.0
  fromIdentifier :: Identifier -> name

{- |

@since 1.0.0.0
-}
instance IdentifierExpression Identifier where
  toIdentifier = id
  fromIdentifier = id
