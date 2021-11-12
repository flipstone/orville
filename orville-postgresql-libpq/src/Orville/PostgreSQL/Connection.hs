{-# LANGUAGE RankNTypes #-}

{- |
Module    : Orville.PostgreSQL.Connection
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Connection
  ( Connection,
    Pool,
    ConnectionUsedAfterCloseError,
    ConnectionError,
    SqlExecutionError (..),
    NoticeReporting (EnableNoticeReporting, DisableNoticeReporting),
    createConnectionPool,
    executeRaw,
    executeRawVoid,
    escapeStringLiteral,
  )
where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, tryReadMVar, tryTakeMVar)
import Control.Exception (Exception, mask, throwIO)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import Data.Pool (Pool, createPool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Internal.PgTextFormatValue (NULByteFoundError (NULByteFoundError), PgTextFormatValue, toBytesForLibPQ)

{- |
  An option for 'createConnectionPool' than indicates whether the LibPQ should
  print notice reports for warnings to the console
-}
data NoticeReporting
  = EnableNoticeReporting
  | DisableNoticeReporting

{- |
 'createConnectionPool' allocates a pool of connections to a PosgreSQL server.
-}
createConnectionPool ::
  -- | Whether or not notice reporting from LibPQ should be enabled
  NoticeReporting ->
  -- | Number of stripes in the connection pool
  Int ->
  -- | Linger time before closing an idle connection
  NominalDiffTime ->
  -- | Max number of connections to allocate per stripe
  Int ->
  -- | A PostgreSQL connection string
  BS.ByteString ->
  IO (Pool Connection)
createConnectionPool noticeReporting stripes linger maxRes connectionString =
  createPool (connect noticeReporting connectionString) close stripes linger maxRes

{- |
 'executeRaw' runs a given SQL statement returning the raw underlying result.

 All handling of stepping through the result set is left to the caller.  This
 potentially leaves connections open much longer than one would expect if all
 of the results are not iterated through immediately *and* the data copied.
 Use with caution.
-}
executeRaw ::
  Connection ->
  BS.ByteString ->
  [Maybe PgTextFormatValue] ->
  IO LibPQ.Result
executeRaw connection bs params =
  case traverse (traverse toBytesForLibPQ) params of
    Left NULByteFoundError ->
      throwIO NULByteFoundError
    Right paramBytes ->
      underlyingExecute bs paramBytes connection

{- |
 'executeRawVoid' a version of 'executeRaw' that completely ignores the result.
 If an error occurs it is raised as an exception.
 Use with caution.
-}
executeRawVoid :: Connection -> BS.ByteString -> [Maybe PgTextFormatValue] -> IO ()
executeRawVoid connection bs params =
  void $ executeRaw connection bs params

{- |
 The basic connection interface.
-}
newtype Connection = Connection (MVar LibPQ.Connection)

{- |
 'connect' is the internal, primitive connection function.

 This should not be exposed to end users, but instead wrapped in something to create a pool.

 Note that handling the libpq connection with the polling is described at
 <https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html>.
-}
connect :: NoticeReporting -> BS.ByteString -> IO Connection
connect noticeReporting connectionString =
  let checkSocketAndThreadWait conn threadWaitFn = do
        fd <- LibPQ.socket conn
        case fd of
          Nothing -> do
            throwConnectionError "connect: failed to get file descriptor for socket" conn
          Just fd' -> do
            threadWaitFn fd'
            poll conn

      poll conn = do
        pollStatus <- LibPQ.connectPoll conn
        case pollStatus of
          LibPQ.PollingFailed -> do
            throwConnectionError "connect: polling failed while connecting to database server" conn
          LibPQ.PollingReading ->
            checkSocketAndThreadWait conn threadWaitRead
          LibPQ.PollingWriting ->
            checkSocketAndThreadWait conn threadWaitWrite
          LibPQ.PollingOk -> do
            connectionHandle <- newMVar conn
            pure (Connection connectionHandle)
   in do
        connection <- LibPQ.connectStart connectionString
        case noticeReporting of
          DisableNoticeReporting -> LibPQ.disableNoticeReporting connection
          EnableNoticeReporting -> LibPQ.enableNoticeReporting connection
        poll connection

{- |
  'close' has many subtleties to it.

  First note that async exceptions are masked.  'mask' though, only works for
  things that are not interruptible
  <https://www.stackage.org/haddock/lts-16.15/base-4.13.0.0/Control-Exception.html#g:13>

  From the previous link, 'tryTakeMVar' is not interruptible, where 'takeMVar'
  *is*.  So by using 'tryTakeMVar' along with 'mask', we should be safe from
  async exceptions causing us to not finish an underlying connection.  Notice
  that the only place the MVar is ever taken is here so 'tryTakeMVar' gives us
  both the non-blocking semantics to protect from async exceptions with 'mask'
  _and_ should never truly return an empty unless two threads were racing to
  close the connection, in which case.. one of them will close the connection.
-}
close :: Connection -> IO ()
close (Connection handle') =
  let underlyingFinish :: (forall a. IO a -> IO a) -> IO (Maybe ())
      underlyingFinish restore = do
        underlyingConnection <- tryTakeMVar handle'
        restore (traverse LibPQ.finish underlyingConnection)
   in void $ mask underlyingFinish

{- |
 'underlyingExecute' is the internal, primitive execute function.

  This is not intended to be directly exposed to end users, but instead wrapped
  in something using a pool.  Note there are potential dragons here in that
  this calls `tryReadMvar` and then returns an error if the MVar is not full.
  The intent is to never expose the ability to empty the `MVar` outside of this
  module, so unless a connection has been closed it *should* never be empty.
  And a connection should be closed upon removal from a resource pool (in which
  case it can't be used for this  function in the first place).
-}
underlyingExecute ::
  BS.ByteString ->
  [Maybe BS.ByteString] ->
  Connection ->
  IO LibPQ.Result
underlyingExecute bs params connection = do
  libPQConn <- readLibPQConnectionOrFailIfClosed connection
  mbResult <-
    LibPQ.execParams libPQConn bs (map mkInferredTextParam params) LibPQ.Text

  case mbResult of
    Nothing -> do
      throwConnectionError "No result returned from exec by libpq" libPQConn
    Just result -> do
      execStatus <- LibPQ.resultStatus result

      if isRowReadableStatus execStatus
        then pure result
        else do
          throwLibPQResultError result execStatus bs

{- |
  Escapes a string for use as a literal within a  SQL command that will be
  execute on the given connection. This uses the @PQescapeStringConn@ function
  from libpq, which takes the character encoding of the connection into
  account. This function only escapes the characters to be used in as a string
  literal -- it does not add the surrounding quotes.
-}
escapeStringLiteral :: Connection -> BS.ByteString -> IO BS.ByteString
escapeStringLiteral connection unescapedString = do
  libPQConn <- readLibPQConnectionOrFailIfClosed connection
  mbEscapedString <- LibPQ.escapeStringConn libPQConn unescapedString

  case mbEscapedString of
    Nothing ->
      throwConnectionError "Error while escaping string literal" libPQConn
    Just escapedString ->
      pure escapedString

readLibPQConnectionOrFailIfClosed :: Connection -> IO LibPQ.Connection
readLibPQConnectionOrFailIfClosed (Connection handle) = do
  mbConn <- tryReadMVar handle

  case mbConn of
    Nothing ->
      throwIO ConnectionUsedAfterCloseError
    Just conn ->
      pure conn

throwConnectionError :: String -> LibPQ.Connection -> IO a
throwConnectionError message conn = do
  mbLibPQError <- LibPQ.errorMessage conn

  throwIO $
    ConnectionError
      { connectionErrorMessage = message
      , connectionErrorLibPQMessage = mbLibPQError
      }

throwLibPQResultError ::
  LibPQ.Result ->
  LibPQ.ExecStatus ->
  BS.ByteString ->
  IO a
throwLibPQResultError result execStatus queryBS = do
  mbLibPQError <- LibPQ.resultErrorMessage result
  mbSqlState <- LibPQ.resultErrorField result LibPQ.DiagSqlstate

  throwIO $
    SqlExecutionError
      { sqlExecutionErrorExecStatus = execStatus
      , sqlExecutionErrorMessage = fromMaybe (B8.pack "No error message available from LibPQ") mbLibPQError
      , sqlExecutionErrorSqlState = mbSqlState
      , sqlExecutionErrorSqlQuery = queryBS
      }

isRowReadableStatus :: LibPQ.ExecStatus -> Bool
isRowReadableStatus status =
  case status of
    LibPQ.CommandOk -> True -- ??
    LibPQ.TuplesOk -> True -- Returned on successful query, even if there are 0 rows.
    LibPQ.SingleTuple -> True -- Only returned when a query is executed is single row mode
    LibPQ.EmptyQuery -> False
    LibPQ.CopyOut -> False
    LibPQ.CopyIn -> False
    LibPQ.CopyBoth -> False -- CopyBoth is only used for streaming replication, so should not occur in ordinary applications
    LibPQ.BadResponse -> False
    LibPQ.NonfatalError -> False -- NonfatalError never returned from LibPQ query execution functions. It passes them to the notice processor instead.
    LibPQ.FatalError -> False

{- |
  Packages a bytestring parameter value (which is assumed to be a value encoded
  as text that the database can use) as a parameter for executing a query.
  This uses Oid 0 to cause the database to infer the type of the paremeter and
  explicitly marks the parameter as being in Text format.
-}
mkInferredTextParam :: Maybe BS.ByteString -> Maybe (LibPQ.Oid, BS.ByteString, LibPQ.Format)
mkInferredTextParam mbValue =
  case mbValue of
    Nothing ->
      Nothing
    Just value ->
      Just (LibPQ.Oid 0, value, LibPQ.Text)

data ConnectionError = ConnectionError
  { connectionErrorMessage :: String
  , connectionErrorLibPQMessage :: Maybe BS.ByteString
  }

instance Show ConnectionError where
  show err =
    let libPQErrorMsg =
          case connectionErrorLibPQMessage err of
            Nothing ->
              "<no underying error available>"
            Just libPQMsg ->
              case Enc.decodeUtf8' libPQMsg of
                Right decoded ->
                  T.unpack decoded
                Left decodingErr ->
                  "Error decoding libPQ messages as utf8: " <> show decodingErr
     in connectionErrorMessage err <> ": " <> libPQErrorMsg

instance Exception ConnectionError

data SqlExecutionError = SqlExecutionError
  { sqlExecutionErrorExecStatus :: LibPQ.ExecStatus
  , sqlExecutionErrorMessage :: BS.ByteString
  , sqlExecutionErrorSqlState :: Maybe BS.ByteString
  , sqlExecutionErrorSqlQuery :: BS.ByteString
  }
  deriving (Show)

instance Exception SqlExecutionError

data ConnectionUsedAfterCloseError
  = ConnectionUsedAfterCloseError
  deriving (Show)

instance Exception ConnectionUsedAfterCloseError
