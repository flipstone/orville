{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.Connection
  ( -- * Orville definitions
    Connection
  , createConnectionPool
  , NoticeReporting (EnableNoticeReporting, DisableNoticeReporting)
  , executeRaw
  , quoteStringLiteral
  , quoteIdentifier
  , ConnectionUsedAfterCloseError
  , ConnectionError
  , SqlExecutionError (..)

    -- * Re-exports from "Data.Pool" for convenience
  , Pool
  )
where

import Control.Concurrent (threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, tryReadMVar, tryTakeMVar)
import Control.Exception (Exception, mask, throwIO)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
#if MIN_VERSION_resource_pool(0,4,0)
import Data.Pool (Pool, newPool, defaultPoolConfig, setNumStripes)
#else
import Data.Pool (Pool, createPool)
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Raw.PgTextFormatValue (NULByteFoundError (NULByteFoundError), PgTextFormatValue, toBytesForLibPQ)

{- |
  An option for 'createConnectionPool' that indicates whether the LibPQ should
  print notice reports for warnings to the console.

@since 1.0.0.0
-}
data NoticeReporting
  = EnableNoticeReporting
  | DisableNoticeReporting

{- |
 'createConnectionPool' allocates a pool of connections to a PostgreSQL server.

@since 1.0.0.0
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
#if MIN_VERSION_resource_pool(0,4,0)
createConnectionPool noticeReporting stripes linger maxRes connectionString =
  newPool . setNumStripes (Just stripes) $
    defaultPoolConfig
      (connect noticeReporting connectionString)
      close
      (realToFrac linger)
      (stripes * maxRes)
#else
createConnectionPool noticeReporting stripes linger maxRes connectionString =
  createPool (connect noticeReporting connectionString) close stripes linger maxRes
#endif

{- |
 'executeRaw' runs a given SQL statement returning the raw underlying result.

 All handling of stepping through the result set is left to the caller.  This
 potentially leaves connections open much longer than one would expect if all
 of the results are not iterated through immediately *and* the data copied.
 Use with caution.

@since 1.0.0.0
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
  An Orville handle for a LibPQ connection.

@since 1.0.0.0
-}
newtype Connection = Connection (MVar LibPQ.Connection)

{- |
 'connect' is the internal, primitive connection function.

 This should not be exposed to end users, but instead wrapped in something to create a pool.

 Note that handling the libpq connection with the polling is described at
 <https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html>.

@since 1.0.0.0
-}
connect :: NoticeReporting -> BS.ByteString -> IO Connection
connect noticeReporting connectionString =
  let
    checkSocketAndThreadWait conn threadWaitFn = do
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
  in
    do
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

@since 1.0.0.0
-}
close :: Connection -> IO ()
close (Connection handle') =
  let
    underlyingFinish :: (forall a. IO a -> IO a) -> IO (Maybe ())
    underlyingFinish restore = do
      underlyingConnection <- tryTakeMVar handle'
      restore (traverse LibPQ.finish underlyingConnection)
  in
    void $ mask underlyingFinish

{- |
 'underlyingExecute' is the internal, primitive execute function.

  This is not intended to be directly exposed to end users, but instead wrapped
  in something using a pool.  Note there are potential dragons here in that
  this calls `tryReadMvar` and then returns an error if the MVar is not full.
  The intent is to never expose the ability to empty the `MVar` outside of this
  module, so unless a connection has been closed it *should* never be empty.
  And a connection should be closed upon removal from a resource pool (in which
  case it can't be used for this  function in the first place).

@since 1.0.0.0
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
      throwExecutionErrorWithoutResult libPQConn bs
    Just result -> do
      execStatus <- LibPQ.resultStatus result

      if isRowReadableStatus execStatus
        then pure result
        else throwExecutionErrorWithResult result execStatus bs

{- |
  Escapes and quotes a string for use as a literal within a SQL command that
  will be execute on the given connection. This uses the @PQescapeStringConn@
  function from libpq, which takes the character encoding of the connection
  into account. Not that while @PQescapeStringConn@ does not surround the
  literal with quotes, this function does for the sake of symmetry with
  'quoteIdentifier'.

  This function returns a `BSB.Buider` so that the result can be included in
  a builder being constructed for the surrounding SQL command without making
  an additional copy of the `BS.Bytestring` returned by LibPQ for the sake of
  adding the surrounding quotes.

@since 1.0.0.0
-}
quoteStringLiteral :: Connection -> BS.ByteString -> IO BSB.Builder
quoteStringLiteral connection unquotedString = do
  libPQConn <- readLibPQConnectionOrFailIfClosed connection
  mbEscapedString <- LibPQ.escapeStringConn libPQConn unquotedString

  case mbEscapedString of
    Nothing ->
      throwConnectionError "Error while escaping string literal" libPQConn
    Just escapedString ->
      let
        singleQuote =
          BSB.char8 '\''
      in
        pure (singleQuote <> BSB.byteString escapedString <> singleQuote)

{- |
  Escapes and quotes a string for use as an identifier within a SQL command
  that will be execute on the given connection. This uses the
  @PQescapeIdentifier@ function from libpq, which takes the character encoding
  of the connection into account and also applies the quotes.

  Although this function does not need to copy the `BS.ByteString` returned by
  LibPQ to add the quotes (since LibPQ already added them), it returns a
  `BSB.Builder` nonetheless to maintain symmetry with `quoteStringLiteral`.

@since 1.0.0.0
-}
quoteIdentifier :: Connection -> BS.ByteString -> IO BSB.Builder
quoteIdentifier connection unquotedString = do
  libPQConn <- readLibPQConnectionOrFailIfClosed connection
  mbEscapedString <- LibPQ.escapeIdentifier libPQConn unquotedString

  case mbEscapedString of
    Nothing ->
      throwConnectionError "Error while escaping identifier" libPQConn
    Just quotedString ->
      pure (BSB.byteString quotedString)

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

throwExecutionErrorWithoutResult ::
  LibPQ.Connection ->
  BS.ByteString ->
  IO a
throwExecutionErrorWithoutResult conn queryBS = do
  mbLibPQError <- LibPQ.errorMessage conn

  throwIO $
    SqlExecutionError
      { sqlExecutionErrorExecStatus = Nothing
      , sqlExecutionErrorMessage = fromMaybe (B8.pack "No error message available from LibPQ") mbLibPQError
      , sqlExecutionErrorSqlState = Nothing
      , sqlExecutionErrorSqlQuery = queryBS
      }

throwExecutionErrorWithResult ::
  LibPQ.Result ->
  LibPQ.ExecStatus ->
  BS.ByteString ->
  IO a
throwExecutionErrorWithResult result execStatus queryBS = do
  mbLibPQError <- LibPQ.resultErrorMessage result
  mbSqlState <- LibPQ.resultErrorField result LibPQ.DiagSqlstate

  throwIO $
    SqlExecutionError
      { sqlExecutionErrorExecStatus = Just execStatus
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

@since 1.0.0.0
-}
mkInferredTextParam :: Maybe BS.ByteString -> Maybe (LibPQ.Oid, BS.ByteString, LibPQ.Format)
mkInferredTextParam mbValue =
  case mbValue of
    Nothing ->
      Nothing
    Just value ->
      Just (LibPQ.Oid 0, value, LibPQ.Text)

{- |
  Orville throws a 'ConnectionError' on an error reported by the underlying
  LibPQ connection that does not come directly from executing SQL. This could
  could represent an inability to open a new database connection, but could
  also represent other errors such as an error while quoting a database
  identifier.

@since 1.0.0.0
-}
data ConnectionError = ConnectionError
  { connectionErrorMessage :: String
  , connectionErrorLibPQMessage :: Maybe BS.ByteString
  }

instance Show ConnectionError where
  show err =
    let
      libPQErrorMsg =
        case connectionErrorLibPQMessage err of
          Nothing ->
            "<no underying error available>"
          Just libPQMsg ->
            case Enc.decodeUtf8' libPQMsg of
              Right decoded ->
                T.unpack decoded
              Left decodingErr ->
                "Error decoding libPQ messages as utf8: " <> show decodingErr
    in
      connectionErrorMessage err <> ": " <> libPQErrorMsg

instance Exception ConnectionError

{- |
  Orville throws a 'SqlExecutionError' when an error is reported by the
  underlying LibPQ connection during an attempt to execute SQL.

@since 1.0.0.0
-}
data SqlExecutionError = SqlExecutionError
  { sqlExecutionErrorExecStatus :: Maybe LibPQ.ExecStatus
  -- ^ The underlying LibPQ execution status
  , sqlExecutionErrorMessage :: BS.ByteString
  -- ^ Error message reported by PostgreSQL
  , sqlExecutionErrorSqlState :: Maybe BS.ByteString
  -- ^ Any SQL state value reported by PostgreSQL. This can be used to
  -- programming determine what kind of error happening without needing to
  -- parse the error message. See
  -- https://www.postgresql.org/docs/current/errcodes-appendix.html
  , sqlExecutionErrorSqlQuery :: BS.ByteString
  -- ^ The SQL query that was being run when the error occurred.
  }
  deriving (Show)

instance Exception SqlExecutionError

{- |
  Orville throws as 'ConnectionUsedAfterCloseError' if it attempts to use a
  'Connection' value after it has already been closed. If this occurs, it is a
  bug in Orville.

@since 1.0.0.0
-}
data ConnectionUsedAfterCloseError
  = ConnectionUsedAfterCloseError
  deriving (Show)

instance Exception ConnectionUsedAfterCloseError
