{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.FieldName
  ( FieldName
  , stringToFieldName
  , fieldNameToString
  , fieldNameToColumnName
  , fieldNameToByteString
  , byteStringToFieldName
  ) where

import qualified Data.ByteString.Char8 as B8

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  A simple type to represent the name of a field.

@since 1.0.0.0
-}
newtype FieldName
  = FieldName B8.ByteString
  deriving (Eq, Ord, Show)

{- |
  Convert a field name to an 'Expr.ColumnName' for usage in SQL expressions.
  The field name will be properly quoted and escaped.

@since 1.0.0.0
-}
fieldNameToColumnName :: FieldName -> Expr.ColumnName
fieldNameToColumnName (FieldName name) =
  Expr.fromIdentifier (Expr.identifierFromBytes name)

{- |
  Constructs a 'FieldName' from a 'String'

@since 1.0.0.0
-}
stringToFieldName :: String -> FieldName
stringToFieldName =
  FieldName . B8.pack

{- |
  Converts a 'FieldName' back to a 'String'

@since 1.0.0.0
-}
fieldNameToString :: FieldName -> String
fieldNameToString =
  B8.unpack . fieldNameToByteString

{- |
  Converts a 'FieldName' back to a 'B8.ByteString'

@since 1.0.0.0
-}
fieldNameToByteString :: FieldName -> B8.ByteString
fieldNameToByteString (FieldName name) =
  name

{- |
  Constructs a 'FieldName' from a 'B8.ByteString'

@since 1.0.0.0
-}
byteStringToFieldName :: B8.ByteString -> FieldName
byteStringToFieldName = FieldName
