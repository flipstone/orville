{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
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

newtype FieldName
  = FieldName B8.ByteString
  deriving (Eq, Ord, Show)

fieldNameToColumnName :: FieldName -> Expr.ColumnName
fieldNameToColumnName (FieldName name) =
  Expr.fromIdentifier (Expr.identifierFromBytes name)

stringToFieldName :: String -> FieldName
stringToFieldName =
  FieldName . B8.pack

fieldNameToString :: FieldName -> String
fieldNameToString =
  B8.unpack . fieldNameToByteString

fieldNameToByteString :: FieldName -> B8.ByteString
fieldNameToByteString (FieldName name) =
  name

byteStringToFieldName :: B8.ByteString -> FieldName
byteStringToFieldName = FieldName
