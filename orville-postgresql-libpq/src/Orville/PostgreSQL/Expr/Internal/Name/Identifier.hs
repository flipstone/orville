{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
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

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a raw (unescaped) identifier by hand and
use it in a place that expected an 'Identifer', that could be done as

 > RawSql.unsafeSqlExpression "my_identifier"

@since 0.10.0.0
-}
newtype Identifier
  = Identifier RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

identifier :: String -> Identifier
identifier =
  identifierFromBytes . B8.pack

identifierFromBytes :: B8.ByteString -> Identifier
identifierFromBytes =
  Identifier . RawSql.identifier

class IdentifierExpression name where
  toIdentifier :: name -> Identifier
  fromIdentifier :: Identifier -> name

instance IdentifierExpression Identifier where
  toIdentifier = id
  fromIdentifier = id
