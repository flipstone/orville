{-|
Module    : Database.Orville.PostgreSQL.Internal.RawSql
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT

The funtions in this module are named with the intent that it is imported
qualified as 'RawSql'.

-}
module Database.Orville.PostgreSQL.Internal.RawSql
  ( RawSql
  , fromString
  , fromBytes
  , execute
  , executeVoid
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Database.PostgreSQL.LibPQ as LibPQ

import           Database.Orville.PostgreSQL.Connection (Connection, Pool)
import qualified Database.Orville.PostgreSQL.Connection as Conn

{-|
  'RawSql' provides a type for efficiently constructing raw sql statements
  from smaller parts and then executing them.
-}
newtype RawSql =
  RawSql BSB.Builder

instance Semigroup RawSql where
  (RawSql left) <> (RawSql right) =
    RawSql (left <> right)

instance Monoid RawSql where
  mempty = RawSql mempty

{-|
  Constructs a 'RawSql' from a 'String' value, utf8-encoding it.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.
-}
fromString :: String -> RawSql
fromString =
  RawSql . BSB.stringUtf8

{-|
  Constructs a 'RawSql' from a 'ByteString' value, which is assumed to be
  encoded sensibly for the database to handle.

  Note that because the string is treated as raw sql it completely up to the
  caller to protected againt sql-injections attacks when using this function.
  Never use this function with input read from an untrusted source.
-}
fromBytes :: BS.ByteString -> RawSql
fromBytes =
  RawSql . BSB.byteString

{-|
  Converts a 'RawSql' value to the bytes that will be sent to the database to
  execute it.
-}
toBytes :: RawSql -> BS.ByteString
toBytes (RawSql builder) =
  LBS.toStrict (BSB.toLazyByteString builder)

{-|
  Executes a 'RawSql' value using the 'Conn.executeRaw' function. Make sure
  to read the documentation of 'Conn.executeRaw' for caveats and warnings.
  Use with caution.
-}
execute :: Pool Connection -> RawSql -> IO (Maybe LibPQ.Result)
execute conn rawSql =
  Conn.executeRaw conn (toBytes rawSql)

{-|
  Executes a 'RawSql' value using the 'Conn.executeRawVoid' function. Make sure
  to read the documentation of 'Conn.executeRawVoid' for caveats and warnings.
  Use with caution.
-}
executeVoid :: Pool Connection -> RawSql -> IO ()
executeVoid conn rawSql =
  Conn.executeRawVoid conn (toBytes rawSql)
