{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.Connection
  ( ConnectionOptions
      ( ConnectionOptions
      , connectionString
      , connectionNoticeReporting
      , connectionPoolStripes
      , connectionPoolLingerTime
      , connectionPoolMaxConnections
      )
  , NoticeReporting (EnableNoticeReporting, DisableNoticeReporting)
  , MaxConnections (MaxConnectionsTotal, MaxConnectionsPerStripe)
  , StripeOption (OneStripePerCapability, StripeCount)
  , ConnectionPool
  , createConnectionPool
  , Connection
  , withPoolConnection
  , executeRaw
  , quoteStringLiteral
  , quoteIdentifier
  , ConnectionUsedAfterCloseError
  , ConnectionError
  , SqlExecutionError (..)
  , transactionStatus
  , transactionStatusOrThrow
  )
where

import Control.Concurrent (getNumCapabilities, threadWaitRead, threadWaitWrite)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Exception (Exception, mask, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as B8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
#if MIN_VERSION_resource_pool(0,4,0)
import Data.Pool (Pool, newPool, defaultPoolConfig, setNumStripes, withResource)
#else
import Data.Pool (Pool, createPool, withResource)
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time (NominalDiffTime)
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Raw.PgTextFormatValue (NULByteFoundError (NULByteFoundError), PgTextFormatValue, toBytesForLibPQ)

{- | An option for 'createConnectionPool' that indicates whether LibPQ should
  print notice reports for warnings to the console.

@since 1.0.0.0
-}
data NoticeReporting
  = EnableNoticeReporting
  | DisableNoticeReporting

{- | Orville always uses a connection pool to manage the number of open connections
to the database. See 'ConnectionConfig' and 'createConnectionPool' to find how
to create a 'ConnectionPool'.

@since 1.0.0.0
-}
newtype ConnectionPool
  = ConnectionPool (Pool Connection)

{- | 'createConnectionPool' allocates a pool of connections to a PostgreSQL server.

@since 1.0.0.0
-}
createConnectionPool :: ConnectionOptions -> IO ConnectionPool
createConnectionPool options = do
  let
    open =
      connect
        (connectionNoticeReporting options)
        (B8.pack $ connectionString options)

    linger =
      connectionPoolLingerTime options

    maxConns =
      connectionPoolMaxConnections options

  stripes <- determineStripeCount (connectionPoolStripes options)

  connPerStripe <-
    case determineConnectionsPerStripe stripes maxConns of
      Right conns -> pure conns
      Left err ->
        throwIO $
          ConnectionError
            { connectionErrorMessage = err
            , connectionErrorLibPQMessage = Nothing
            }

#if MIN_VERSION_resource_pool(0,4,0)
  fmap ConnectionPool . newPool . setNumStripes (Just stripes) $
    defaultPoolConfig
      open
      close
      (realToFrac linger)
      (stripes * connPerStripe)
#else
  ConnectionPool <$>
    createPool
      open
      close
      stripes
      linger
      connPerStripe
#endif

{- | Values for the 'connectionPoolStripes' field of 'ConnectionOptions'.

@since 1.0.0.0
-}
data StripeOption
  = -- | 'OneStripePerCapability' will cause the connection pool to be set up
    -- with one stripe for each capability (processor thread) available to the
    -- runtime. This is the best option for multi-threaded connection pool
    -- performance.
    OneStripePerCapability
  | -- | 'StripeCount' will cause the connection pool to be set up with
    -- the specified number of stripes, regardless of how many capabilities
    -- the runtime has.
    StripeCount Int

{- | Values for the 'connectionMaxConnections' field of 'ConnectionOptions'.

@since 1.0.0.0
-}
data MaxConnections
  = -- | 'MaxConnectionsTotal' creates a connection pool that will never
    -- allocate more than the specified number of connections. The total count
    -- of connections will be spread evenly across the all the stripes in the
    -- pool. If the number of stripes does not divide the total count evenly,
    -- any remainder will be unused.
    MaxConnectionsTotal Int
  | -- | 'MaxConnectionsPerStripe' creates a connection pool that will
    -- allocate up to the specified number of connections in each stripe.
    -- In this case the total possible number of simultaneous connections will
    -- be this value multiplied by the number of stripes.
    MaxConnectionsPerStripe Int

{- | Configuration options to pass to 'createConnectionPool' to specify the
parameters for the pool and the connections that it creates.

@since 1.0.0.0
-}
data ConnectionOptions = ConnectionOptions
  { connectionString :: String
  -- ^ A PostgreSQL connection string.
  , connectionNoticeReporting :: NoticeReporting
  -- ^ Whether or not notice reporting from LibPQ should be enabled.
  , connectionPoolStripes :: StripeOption
  -- ^ Number of stripes in the connection pool.
  , connectionPoolLingerTime :: NominalDiffTime
  -- ^ Linger time before closing an idle connection.
  , connectionPoolMaxConnections :: MaxConnections
  -- ^ Controls the number of connections available in the 'ConnectionPool'.
  }

-- | INTERNAL: Resolves the 'StripeOption' to the actual number of stripes to use.
determineStripeCount :: StripeOption -> IO Int
determineStripeCount stripeOption =
  case stripeOption of
    OneStripePerCapability -> getNumCapabilities
    StripeCount n -> pure n

{- | INTERNAL: Resolves the 'MaxConnections' to the actual number of connections
  to use per stripe.
-}
determineConnectionsPerStripe :: Int -> MaxConnections -> Either String Int
determineConnectionsPerStripe stripes maxConnections =
  case maxConnections of
    MaxConnectionsPerStripe n ->
      Right n
    MaxConnectionsTotal n ->
      if n >= stripes
        then Right (n `div` stripes)
        else
          Left $
            "Invalid connection pool options. There must be at least "
              <> " 1 connection per stripe, but MaxConnectionsTotal was "
              <> show n
              <> " for "
              <> show stripes
              <> " stripes."

{- | Allocates a connection from the pool and performs an action with it. This
  function will block if the maximum number of connections is reached.

@since 1.0.0.0
-}
withPoolConnection :: ConnectionPool -> (Connection -> IO a) -> IO a
withPoolConnection (ConnectionPool pool) =
  withResource pool

{- | 'executeRaw' runs a given SQL statement returning the raw underlying result.

 All handling of stepping through the result set is left to the caller. This
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

data ConnectionState
  = OpenConnection LibPQ.Connection
  | ClosedConnection

data ConnectionContext = ConnectionContext
  { i_connUtilizationLock :: MVar ()
  -- ^ Used to serialize access to the connection for the purpose of issuing commands
  , i_connCloseLock :: MVar ()
  -- ^ Used to guarantee that only one thread will close the connection. This
  -- is separate from the utilization lock because a connection should still
  -- be closeable if it is in use by another thread but should not be closed
  -- by multiple threads simultaneously.
  , i_connState :: ConnectionState
  -- ^ The underlying connection, if open
  }

{- | An Orville handler for a LibPQ connection.

@since 1.0.0.0
-}
newtype Connection = Connection (IORef ConnectionContext)

{- | 'connect' is the internal, primitive connection function.

 This should not be exposed to end users, but instead wrapped in something to create a pool.

 Note that handling the LibPQ connection with the polling is described at
 <https://hackage.haskell.org/package/postgresql-libpq-0.9.4.2/docs/Database-PostgreSQL-LibPQ.html>.

@since 1.0.0.0
-}
connect :: NoticeReporting -> BS.ByteString -> IO Connection
connect noticeReporting connString =
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
          connUseLock <- newMVar ()
          connCloseLock <- newMVar ()
          connectionHandle <-
            newIORef
              ConnectionContext
                { i_connUtilizationLock = connUseLock
                , i_connCloseLock = connCloseLock
                , i_connState = OpenConnection conn
                }
          pure (Connection connectionHandle)
  in
    do
      connection <- LibPQ.connectStart connString
      case noticeReporting of
        DisableNoticeReporting -> LibPQ.disableNoticeReporting connection
        EnableNoticeReporting -> LibPQ.enableNoticeReporting connection
      poll connection

{- | 'close' has many subtleties to it.

  First note that async exceptions are masked.  'mask' though, only works for
  things that are not interruptible
  <https://www.stackage.org/haddock/lts-16.15/base-4.13.0.0/Control-Exception.html#g:13>

  From the previous link, 'tryTakeMVar' is not interruptible, where @takeMVar@
  *is*.  So by using 'tryTakeMVar' along with 'mask', we should be safe from
  async exceptions causing us to not finish an underlying connection.  Notice
  that the only place the close lock MVar is ever taken is here so
  'tryTakeMVar' gives us both the non-blocking semantics to protect from async
  exceptions with 'mask' _and_ should never truly return an empty unless two
  threads were racing to close the connection, in which case.. one of them will
  close the connection.

@since 1.0.0.0
-}
close :: Connection -> IO ()
close (Connection handle) =
  let
    underlyingFinish :: (forall a. IO a -> IO a) -> IO ()
    underlyingFinish restore = do
      connCtx <- readIORef handle
      mCloseLock <- tryTakeMVar $ i_connCloseLock connCtx
      case (mCloseLock, i_connState connCtx) of
        (Just (), OpenConnection underlyingConnection) -> do
          writeIORef handle connCtx {i_connState = ClosedConnection}
          restore (LibPQ.finish underlyingConnection)
        _ -> pure ()
  in
    mask underlyingFinish

{- | 'underlyingExecute' is the internal, primitive execute function.

  This is not intended to be directly exposed to end users, but instead wrapped
  in something using a pool.

@since 1.0.0.0
-}
underlyingExecute ::
  BS.ByteString ->
  [Maybe BS.ByteString] ->
  Connection ->
  IO LibPQ.Result
underlyingExecute bs params connection =
  withLibPQConnectionOrFailIfClosed connection $ \libPQConn -> do
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

{- | Escapes and quotes a string for use as a literal within a SQL command that
  will be executed on the given connection. This uses the @PQescapeStringConn@
  function from LibPQ, which takes the character encoding of the connection
  into account. Note that while @PQescapeStringConn@ does not surround the
  literal with quotes, this function does for the sake of symmetry with
  'quoteIdentifier'.

  This function returns a `BSB.Builder` so that the result can be included in
  a builder being constructed for the surrounding SQL command without making
  an additional copy of the `BS.ByteString` returned by LibPQ for the sake of
  adding the surrounding quotes.

@since 1.0.0.0
-}
quoteStringLiteral :: Connection -> BS.ByteString -> IO BSB.Builder
quoteStringLiteral connection unquotedString =
  withLibPQConnectionOrFailIfClosed connection $ \libPQConn -> do
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

{- | Escapes and quotes a string for use as an identifier within a SQL command
  that will be executed on the given connection. This uses the
  @PQescapeIdentifier@ function from LibPQ, which takes the character encoding
  of the connection into account and also applies the quotes.

  Although this function does not need to copy the `BS.ByteString` returned by
  LibPQ to add the quotes (since LibPQ already added them), it returns a
  `BSB.Builder` nonetheless to maintain symmetry with `quoteStringLiteral`.

@since 1.0.0.0
-}
quoteIdentifier :: Connection -> BS.ByteString -> IO BSB.Builder
quoteIdentifier connection unquotedString =
  withLibPQConnectionOrFailIfClosed connection $ \libPQConn -> do
    mbEscapedString <- LibPQ.escapeIdentifier libPQConn unquotedString

    case mbEscapedString of
      Nothing ->
        throwConnectionError "Error while escaping identifier" libPQConn
      Just quotedString ->
        pure (BSB.byteString quotedString)

{- | Serializes access to the underlying LibPQ connection. This is necessary
  because multiple concurrent commands issued using the same connection will
  result in a dead-lock in LibPQ.

  Do not nest calls to this function with the same connection or it will
  dead-lock on the MVar.
-}
withLibPQConnectionOrFailIfClosed :: Connection -> (LibPQ.Connection -> IO a) -> IO a
withLibPQConnectionOrFailIfClosed (Connection handle) withConnection = do
  connCtx <- readIORef handle
  withMVar (i_connUtilizationLock connCtx) $ \() ->
    case i_connState connCtx of
      ClosedConnection ->
        throwIO ConnectionUsedAfterCloseError
      OpenConnection conn ->
        withConnection conn

{- |
  Returns 'Just' the transaction status of the current connection, as reported by @libpq@, or
  'Nothing' if the connection is currently closed. See 'Database.PostgreSQL.LibPQ.transactionStatus',
  'Database.PostgreSQL.LibPQ.TransactionStatus' for more information.

@since 1.1.0.0
-}
transactionStatus :: Connection -> IO (Maybe LibPQ.TransactionStatus)
transactionStatus (Connection handle) = do
  connCtx <- readIORef handle
  withMVar (i_connUtilizationLock connCtx) $ \() ->
    case i_connState connCtx of
      ClosedConnection ->
        pure Nothing
      OpenConnection conn ->
        fmap Just (LibPQ.transactionStatus conn)

{- |
  Similar to 'transactionStatus', but throws a 'ConnectionUsedAfterCloseError' if the connection is closed.

@since 1.1.0.0
-}
transactionStatusOrThrow :: Connection -> IO LibPQ.TransactionStatus
transactionStatusOrThrow conn =
  withLibPQConnectionOrFailIfClosed conn LibPQ.transactionStatus

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
#if MIN_VERSION_postgresql_libpq(0,11,0)
    LibPQ.PipelineSync -> False
    LibPQ.PipelineAbort -> False
#endif

{- | Packages a bytestring parameter value (which is assumed to be a value encoded
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

{- | Orville throws a 'ConnectionError' on an error reported by the underlying
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

-- | @since 1.0.0.0
instance Exception ConnectionError

{- | Orville throws a 'SqlExecutionError' when an error is reported by the
  underlying LibPQ connection during an attempt to execute SQL.

@since 1.0.0.0
-}
data SqlExecutionError = SqlExecutionError
  { sqlExecutionErrorExecStatus :: Maybe LibPQ.ExecStatus
  -- ^ The underlying LibPQ execution status.
  , sqlExecutionErrorMessage :: BS.ByteString
  -- ^ Error message reported by PostgreSQL.
  , sqlExecutionErrorSqlState :: Maybe BS.ByteString
  -- ^ Any SQL state value reported by PostgreSQL. This can be used to
  -- determine what kind of error happened without needing to parse the error
  -- message. See
  -- https://www.postgresql.org/docs/current/errcodes-appendix.html.
  , sqlExecutionErrorSqlQuery :: BS.ByteString
  -- ^ The SQL query that was being run when the error occurred.
  }
  deriving
    ( -- | @since 1.0.0.0
      Show
    )

-- | @since 1.0.0.0
instance Exception SqlExecutionError

{- | Orville throws as 'ConnectionUsedAfterCloseError' if it attempts to use a
  'Connection' value after it has already been closed. If this occurs, it is a
  bug in Orville.

@since 1.0.0.0
-}
data ConnectionUsedAfterCloseError
  = ConnectionUsedAfterCloseError
  deriving
    ( -- | @since 1.0.0.0
      Show
    )

-- | @since 1.0.0.0
instance Exception ConnectionUsedAfterCloseError
