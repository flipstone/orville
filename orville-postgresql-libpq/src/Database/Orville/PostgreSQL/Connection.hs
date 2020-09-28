{-|
Module    : Database.Orville.PostgreSQL.Connection
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Connection
  (createConnectionPool)
where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Exception(Exception, mask, throwIO)
import Control.Monad (void)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Data.ByteString (ByteString)
import Data.Pool (Pool, createPool)
import Data.Time (NominalDiffTime)

{-|
 'createConnectionPool' allocates a pool of connections to a PosgreSQL
 server.
-}
createConnectionPool ::
     Int -- ^ Number of stripes in the connection pool
  -> NominalDiffTime -- ^ Linger time before closing an idle connection
  -> Int -- ^ Max number of connections to allocate per stripe
  -> ByteString -- ^ A PostgreSQL connection string
  -> IO (Pool Connection)
createConnectionPool stripes linger maxRes connectionString =
  createPool (connect connectionString) close stripes linger maxRes

{-|
 The basic connection interface.
-}
newtype Connection = Connection (MVar LibPQ.Connection)

{-|
 `connect` is the internal, primitive connection function.
 This should not be exposed to end users, but instead wrapped in something to create a pool.
 Note that handling the libpq connection with the polling is described at <https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html>.
-}
connect :: ByteString -> IO Connection
connect connectionString = do
  connection <- LibPQ.connectStart connectionString
  poll connection
  where
    checkSocketAndThreadWait conn threadWaitFn = do
      fd <- LibPQ.socket conn
      case fd of
        Nothing -> do
          libPQError <- LibPQ.errorMessage conn
          throwIO $ ConnectionError { errorMessage = "failed to get file descriptor"
                                    , underlyingError = libPQError
                                    }
        Just fd' -> do
          threadWaitFn fd'
          poll conn
    poll conn = do
      pollStatus <- LibPQ.connectPoll conn
      case pollStatus of
        LibPQ.PollingFailed -> do
          libPQError <- LibPQ.errorMessage conn
          throwIO $ ConnectionError { errorMessage = "connection failure"
                                    , underlyingError = libPQError
                                    }
        LibPQ.PollingReading -> checkSocketAndThreadWait conn threadWaitRead
        LibPQ.PollingWriting -> checkSocketAndThreadWait conn threadWaitWrite
        LibPQ.PollingOk -> do
          connectionHandle <- newMVar conn
          pure (Connection connectionHandle)

{-|
  `close` has many subtleties to it.
  First note that async exceptions are masked.
  `mask` though, only works for things that are not interruptible <https://www.stackage.org/haddock/lts-16.15/base-4.13.0.0/Control-Exception.html#g:13>
  From the previous link, `tryTakeMVar` is not interruptible, where `takeMVar` *is*.
  So by using `tryTakeMVar` along with `mask`, we should be safe from async exceptions causing us to not finish an underlying connection.
  Notice that the only place the MVar is ever taken is here so `tryTakeMVar` gives us both the non-blocking semantics to protect from async exceptions with `mask` _and_ should never truly return an empty unless two threads were racing to close the connection, in which case.. one of them will close the connection.

-}
close :: Connection -> IO ()
close (Connection handle') =
  let underlyingFinish restore = do
        underlyingConnection <- tryTakeMVar handle'
        restore (traverse LibPQ.finish underlyingConnection)
  in
    void $ mask underlyingFinish


data ConnectionError = ConnectionError { errorMessage :: String
                                       , underlyingError :: Maybe ByteString
                                       }

instance Show ConnectionError where
  show x = let libPQErrorMsg = maybe "" ((<>) ": " . show ) $ underlyingError x
           in
             (errorMessage x) <> libPQErrorMsg

instance Exception ConnectionError
