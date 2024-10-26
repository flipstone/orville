{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Marshall.AliasName
  ( AliasName
  , stringToAliasName
  , aliasNameToString
  , aliasNameToAliasExpr
  , aliasNameAndFieldNameToColumnName
  , aliasNameToByteString
  , byteStringToAliasName
  , aliasNameAsFieldName
  ) where

import qualified Data.ByteString.Char8 as B8

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Internal.FieldName (FieldName, byteStringToFieldName, fieldNameToColumnName)

{- | A simple type to represent the name of a field.

@since 1.1.0.0
-}
newtype AliasName
  = AliasName B8.ByteString
  deriving
    ( -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    , -- | @since 1.1.0.0
      Show
    )

{- | Convert an 'AliasName' and a 'FieldName' to a 'Expr.ColumnName' for usage in SQL expressions.
  The result will be properly quoted and escaped.

@since 1.1.0.0
-}
aliasNameAndFieldNameToColumnName :: AliasName -> FieldName -> Expr.Qualified Expr.ColumnName
aliasNameAndFieldNameToColumnName aliasName =
  Expr.aliasQualifyColumn
    (aliasNameToAliasExpr aliasName)
    . fieldNameToColumnName

{- | Constructs a 'AliasName' from a 'String'.

@since 1.1.0.0
-}
stringToAliasName :: String -> AliasName
stringToAliasName =
  AliasName . B8.pack

{- | Converts a 'AliasName' to an 'AliasExpr'.

@since 1.1.0.0
-}
aliasNameToAliasExpr :: AliasName -> Expr.AliasExpr
aliasNameToAliasExpr =
  Expr.fromIdentifier . Expr.identifierFromBytes . aliasNameToByteString

{- | Converts a 'AliasName' back to a 'String'.

@since 1.1.0.0
-}
aliasNameToString :: AliasName -> String
aliasNameToString =
  B8.unpack . aliasNameToByteString

{- | Converts a 'AliasName' back to a 'B8.ByteString'.

@since 1.1.0.0
-}
aliasNameToByteString :: AliasName -> B8.ByteString
aliasNameToByteString (AliasName name) =
  name

{- | Constructs a 'AliasName' from a 'B8.ByteString'.

@since 1.1.0.0
-}
byteStringToAliasName :: B8.ByteString -> AliasName
byteStringToAliasName = AliasName

{- | Allows to treat an 'AliasName' as a 'FieldName'.

@since 1.1.0.0
-}
aliasNameAsFieldName :: AliasName -> FieldName
aliasNameAsFieldName =
  byteStringToFieldName . aliasNameToByteString
