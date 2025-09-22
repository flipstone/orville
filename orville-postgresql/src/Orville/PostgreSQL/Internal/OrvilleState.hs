{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.OrvilleState
  ( OrvilleState
  , newOrvilleState
  , resetOrvilleState
  , orvilleConnectionPool
  , orvilleConnectionState
  , orvilleErrorDetailLevel
  , orvilleTransactionCallback
  , orvilleSqlCommenterAttributes
  , addTransactionCallback
  , TransactionEvent (BeginTransaction, NewSavepoint, ReleaseSavepoint, RollbackToSavepoint, CommitTransaction, RollbackTransaction)
  , openTransactionEvent
  , rollbackTransactionEvent
  , transactionSuccessEvent
  , ConnectionState (NotConnected, Connected)
  , ConnectedState (ConnectedState, connectedConnection, connectedTransaction)
  , connectState
  , TransactionState (OutermostTransaction, SavepointTransaction)
  , newTransaction
  , Savepoint
  , savepointNestingLevel
  , initialSavepoint
  , nextSavepoint
  , orvilleSqlExecutionCallback
  , addSqlExecutionCallback
  , orvilleBeginTransactionExpr
  , setBeginTransactionExpr
  , setSqlCommenterAttributes
  , addSqlCommenterAttributes
  )
where

import qualified Data.Map.Strict as Map

import Orville.PostgreSQL.ErrorDetailLevel (ErrorDetailLevel)
import Orville.PostgreSQL.Execution.QueryType (QueryType)
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Raw.Connection (Connection, ConnectionPool)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlCommenter as SqlCommenter

{- | 'OrvilleState' is used to manage opening connections to the database,
  transactions, etc. 'newOrvilleState' should be used to create an appopriate
  initial state for your monad's context.

@since 1.0.0.0
-}
data OrvilleState = OrvilleState
  { _orvilleConnectionPool :: ConnectionPool
  , _orvilleConnectionState :: ConnectionState
  , _orvilleErrorDetailLevel :: ErrorDetailLevel
  , _orvilleTransactionCallback :: TransactionEvent -> IO ()
  , _orvilleSqlExecutionCallback :: forall a. QueryType -> RawSql.RawSql -> IO a -> IO a
  , _orvilleBeginTransactionExpr :: Expr.BeginTransactionExpr
  , _orvilleSqlCommenterAttributes :: Maybe SqlCommenter.SqlCommenterAttributes
  }

{- | Get the connection pool being used for the 'OrvilleState'.

@since 1.0.0.0
-}
orvilleConnectionPool :: OrvilleState -> ConnectionPool
orvilleConnectionPool =
  _orvilleConnectionPool

{- | INTERNAL: The 'ConnectionState' indicates whether Orville currently has a
  connection open, and contains the connection if it does.

@since 1.0.0.0
-}
orvilleConnectionState :: OrvilleState -> ConnectionState
orvilleConnectionState =
  _orvilleConnectionState

{- | The 'ErrorDetailLevel' controls how much information Orville includes in
  error messages it generates when data cannot be decoded from rows in the
  database.

@since 1.0.0.0
-}
orvilleErrorDetailLevel :: OrvilleState -> ErrorDetailLevel
orvilleErrorDetailLevel =
  _orvilleErrorDetailLevel

{- | Orville will call the transaction callback any time a transaction event
  occurs. You can register a callback with 'addTransactionCallback'.

@since 1.0.0.0
-}
orvilleTransactionCallback :: OrvilleState -> TransactionEvent -> IO ()
orvilleTransactionCallback =
  _orvilleTransactionCallback

{- | The SQL expression that Orville will use to begin a transaction. You can set
  this via 'setBeginTransactionExpr' to have fine-grained control over the
  transaction parameters, such as isolation level.

@since 1.0.0.0
-}
orvilleBeginTransactionExpr :: OrvilleState -> Expr.BeginTransactionExpr
orvilleBeginTransactionExpr =
  _orvilleBeginTransactionExpr

{- | The SqlCommenter attributes that Orville will include with queries. These can
  be modified with 'addSqlCommenterAttributes'. See
  https://google.github.io/sqlcommenter/.

@since 1.0.0.0
-}
orvilleSqlCommenterAttributes :: OrvilleState -> Maybe SqlCommenter.SqlCommenterAttributes
orvilleSqlCommenterAttributes =
  _orvilleSqlCommenterAttributes

{- | Registers a callback to be invoked during transactions.

  The callback given will be called after the SQL statement corresponding
  to the given event has finished executing. Callbacks will be called
  in the order they are added.

  Note: There is no specialized error handling for these callbacks. This means
  that if a callback raises an exception, no further callbacks will be called
  and the exception will propagate up until it is caught elsewhere. In
  particular, if an exception is raised by a callback upon opening the
  transaction, it will cause the transaction to be rolled-back the same as any
  other exception that might happen during the transaction. In general, we
  recommend only using callbacks that either raise no exceptions or can handle
  their own exceptions cleanly.

@since 1.0.0.0
-}
addTransactionCallback ::
  (TransactionEvent -> IO ()) ->
  OrvilleState ->
  OrvilleState
addTransactionCallback newCallback state =
  let
    originalCallback =
      _orvilleTransactionCallback state

    wrappedCallback event = do
      originalCallback event
      newCallback event
  in
    state {_orvilleTransactionCallback = wrappedCallback}

{- | Creates an appropriate initial 'OrvilleState' that will use the connection
  pool given to initiate connections to the database.

@since 1.0.0.0
-}
newOrvilleState :: ErrorDetailLevel -> ConnectionPool -> OrvilleState
newOrvilleState errorDetailLevel pool =
  OrvilleState
    { _orvilleConnectionPool = pool
    , _orvilleConnectionState = NotConnected
    , _orvilleErrorDetailLevel = errorDetailLevel
    , _orvilleTransactionCallback = defaultTransactionCallback
    , _orvilleSqlExecutionCallback = defaultSqlExectionCallback
    , _orvilleBeginTransactionExpr = defaultBeginTransactionExpr
    , _orvilleSqlCommenterAttributes = Nothing
    }

{- | Creates a new initial 'OrvilleState' using the connection pool from the
  provided state. You might need to use this if you are spawning one Orville
  monad from another and they should not share the same connection and
  transaction state.

@since 1.0.0.0
-}
resetOrvilleState :: OrvilleState -> OrvilleState
resetOrvilleState =
  newOrvilleState
    <$> _orvilleErrorDetailLevel
    <*> _orvilleConnectionPool

{- | INTERNAL: Transitions the 'OrvilleState' into "connected" status, storing the
  given 'Connection' as the database connection to be used to execute all
  queries. This is used by 'Orville.PostgreSQL.Monad.withConnection' to track
  the connection it retrieves from the pool.

@since 1.0.0.0
-}
connectState :: ConnectedState -> OrvilleState -> OrvilleState
connectState connectedState state =
  state
    { _orvilleConnectionState = Connected connectedState
    }

{- | INTERNAL: This type is used to signal whether a database connection has
  been retrieved from the pool for the current operation or not. The
  value is tracked in the 'OrvilleState' for the host monad, and is checked
  by 'Orville.PostgreSQL.Monad.withConnection' to avoid checking out two
  separate connections for a multiple operations that needs to be run on the
  same connection (e.g. multiple operations inside a transaction).

@since 1.0.0.0
-}
data ConnectionState
  = NotConnected
  | Connected ConnectedState

{- | INTERNAL: This type is used hold the connection while it is open and
  track the state of open transactions and savepoints on the connection.

@since 1.0.0.0
-}
data ConnectedState = ConnectedState
  { connectedConnection :: Connection
  , connectedTransaction :: Maybe TransactionState
  }

{- | INTERNAL: This type is use to track the state of open transactions and
  savepoints on an open connection.

@since 1.0.0.0
-}
data TransactionState
  = OutermostTransaction
  | SavepointTransaction Savepoint

{- | INTERNAL: Constructs a new 'TransactionState' to represent beginning a
  new transaction in SQL.

@since 1.0.0.0
-}
newTransaction :: Maybe TransactionState -> TransactionState
newTransaction maybeTransactionState =
  case maybeTransactionState of
    Nothing ->
      OutermostTransaction
    Just OutermostTransaction ->
      SavepointTransaction initialSavepoint
    Just (SavepointTransaction savepoint) ->
      SavepointTransaction (nextSavepoint savepoint)

{- | An internal Orville identifier for a savepoint in a PostgreSQL transaction.

@since 1.0.0.0
-}
newtype Savepoint
  = Savepoint Int
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Show
    )

{- | The initial identifier Orville uses to track the first savepoint within
  a transaction.

@since 1.0.0.0
-}
initialSavepoint :: Savepoint
initialSavepoint =
  Savepoint 1

{- | Determines the identifier for the next savepoint in a transaction after the
  given savepoint.

@since 1.0.0.0
-}
nextSavepoint :: Savepoint -> Savepoint
nextSavepoint (Savepoint n) =
  Savepoint (n + 1)

{- | Indicates how many levels of nested savepoints the given 'Savepoint'
  identifier represents.

@since 1.0.0.0
-}
savepointNestingLevel :: Savepoint -> Int
savepointNestingLevel (Savepoint n) = n

{- | Describes an event in the lifecycle of a database transaction. You can use
  'addTransactionCallback' to register a callback to respond to these events.
  The callback will be called after the event in question has been successfully
  executed.

@since 1.0.0.0
-}
data TransactionEvent
  = -- | Indicates a new transaction has been started.
    BeginTransaction
  | -- | Indicates that a new savepoint has been saved within a transaction.
    NewSavepoint Savepoint
  | {- | Indicates that a previous savepoint has been released. It can no
    longer be rolled back to.
    -}
    ReleaseSavepoint Savepoint
  | {- | Indicates that rollback was performed to a prior savepoint.

    Note: It is possible to rollback to a savepoint prior to the most recent
    one without releasing or rolling back to intermediate savepoints. Doing
    so destroys any savepoints created after the given savepoint. Although
    Orville currently always matches 'NewSavepoint' with either
    'ReleaseSavepoint' or 'RollbackToSavepoint', it is recommended that you
    do not rely on this behavior.
    -}
    RollbackToSavepoint Savepoint
  | -- | Indicates that the transaction has been committed.
    CommitTransaction
  | -- | Indicates that the transaction has been rolled back.
    RollbackTransaction
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Show
    )

{- | The default transaction callback is simply a no-op.

@since 1.0.0.0
-}
defaultTransactionCallback :: TransactionEvent -> IO ()
defaultTransactionCallback = const (pure ())

{- | Constructs the appropriate 'TransactionEvent' for opening a new transaction
  based on the current 'TransactionState'.

@since 1.0.0.0
-}
openTransactionEvent :: TransactionState -> TransactionEvent
openTransactionEvent txnState =
  case txnState of
    OutermostTransaction -> BeginTransaction
    SavepointTransaction savepoint -> NewSavepoint savepoint

{- | Constructs the appropriate 'TransactionEvent' for rolling back the innermost
  transaction based on the current 'TransactionState'.

@since 1.0.0.0
-}
rollbackTransactionEvent :: TransactionState -> TransactionEvent
rollbackTransactionEvent txnState =
  case txnState of
    OutermostTransaction -> RollbackTransaction
    SavepointTransaction savepoint -> RollbackToSavepoint savepoint

{- | Constructs the appropriate 'TransactionEvent' to represent a transaction
  completely successfully based on the current 'TransactionState'.

@since 1.0.0.0
-}
transactionSuccessEvent :: TransactionState -> TransactionEvent
transactionSuccessEvent txnState =
  case txnState of
    OutermostTransaction -> CommitTransaction
    SavepointTransaction savepoint -> ReleaseSavepoint savepoint

{- | The callback Orville will call whenever it wants to run SQL. You can
  register a callback using 'addSqlExecutionCallback'.

@since 1.0.0.0
-}
orvilleSqlExecutionCallback ::
  OrvilleState ->
  forall a.
  QueryType ->
  RawSql.RawSql ->
  IO a ->
  IO a
orvilleSqlExecutionCallback =
  _orvilleSqlExecutionCallback

{- | The default SQL execption callback simply runs the IO action given without
  doing anything else.

@since 1.0.0.0
-}
defaultSqlExectionCallback :: QueryType -> RawSql.RawSql -> IO a -> IO a
defaultSqlExectionCallback _ _ io = io

{- | Adds a callback to be called when an Orville operation executes a SQL
  statement. The callback is given the IO action that will perform the
  query execution and must call that action for the query to be run.
  In particular, you can use this to time queries and log any that are slow.

  Calls to any previously added callbacks will also be executed as part of
  the IO action passed to the new callback. Thus the newly added callback
  happens "around" the previously added callback.

  There is no special exception handling done for these callbacks beyond what
  they implement themselves. Any callbacks should allow for the possibility
  that the IO action they are given may raise an exception.

@since 1.0.0.0
-}
addSqlExecutionCallback ::
  (forall a. QueryType -> RawSql.RawSql -> IO a -> IO a) ->
  OrvilleState ->
  OrvilleState
addSqlExecutionCallback outerCallback state =
  let
    layeredCallback, innerCallback :: QueryType -> RawSql.RawSql -> IO a -> IO a
    layeredCallback queryType sql action =
      outerCallback queryType sql (innerCallback queryType sql action)
    innerCallback = _orvilleSqlExecutionCallback state
  in
    state {_orvilleSqlExecutionCallback = layeredCallback}

{- | The default begin transaction expression is simply @BEGIN TRANSACTION@
  with no options specified.

@since 1.0.0.0
-}
defaultBeginTransactionExpr :: Expr.BeginTransactionExpr
defaultBeginTransactionExpr =
  Expr.beginTransaction Nothing

{- | Sets the SQL expression that Orville will use to begin transactions. You can
  control the transaction isolation level by building your own
  'Expr.BeginTransactionExpr' with the desired isolation level.

@since 1.0.0.0
-}
setBeginTransactionExpr ::
  Expr.BeginTransactionExpr ->
  OrvilleState ->
  OrvilleState
setBeginTransactionExpr expr state =
  state
    { _orvilleBeginTransactionExpr = expr
    }

{- | Sets the SqlCommenterAttributes that Orville will then add to any following
  statement executions.

@since 1.0.0.0
-}
setSqlCommenterAttributes ::
  SqlCommenter.SqlCommenterAttributes ->
  OrvilleState ->
  OrvilleState
setSqlCommenterAttributes comments state =
  state
    { _orvilleSqlCommenterAttributes = Just comments
    }

{- | Adds the SqlCommenterAttributes to the already existing attributes that
  Orville will then add to any following statement executions.

@since 1.0.0.0
-}
addSqlCommenterAttributes ::
  SqlCommenter.SqlCommenterAttributes ->
  OrvilleState ->
  OrvilleState
addSqlCommenterAttributes comments state =
  case orvilleSqlCommenterAttributes state of
    Nothing ->
      state
        { _orvilleSqlCommenterAttributes = Just comments
        }
    Just existingAttrs ->
      state
        { _orvilleSqlCommenterAttributes = Just $ Map.union comments existingAttrs
        }
