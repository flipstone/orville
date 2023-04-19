{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.Identifier
  ( Identifier,
    identifier,
    identifierFromBytes,
    unquotedIdentifier,
    unquotedIdentifierFromBytes,
    IdentifierExpression (toIdentifier, fromIdentifier),
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype Identifier
  = Identifier RawSql.RawSql
  deriving (RawSql.SqlExpression)

identifier :: String -> Identifier
identifier =
  identifierFromBytes . B8.pack

identifierFromBytes :: B8.ByteString -> Identifier
identifierFromBytes idBytes =
  Identifier $
    RawSql.doubleQuote <> RawSql.fromBytes idBytes <> RawSql.doubleQuote

unquotedIdentifier :: String -> Identifier
unquotedIdentifier =
  unquotedIdentifierFromBytes . B8.pack

unquotedIdentifierFromBytes :: B8.ByteString -> Identifier
unquotedIdentifierFromBytes =
  Identifier . RawSql.fromBytes

class IdentifierExpression name where
  toIdentifier :: name -> Identifier
  fromIdentifier :: Identifier -> name

instance IdentifierExpression Identifier where
  toIdentifier = id
  fromIdentifier = id
