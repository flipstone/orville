{-# LANGUAGE FlexibleInstances #-}

module Orville.PostgreSQL.Internal.OrvilleState
  ( OrvilleState,
    newOrvilleState,
    resetOrvilleState,
    orvilleConnectionPool,
    orvilleConnectionState,
    orvilleErrorDetailLevel,
    orvilleTransactionCallback,
    addTransactionCallback,
    TransactionEvent (BeginTransaction, NewSavepoint, ReleaseSavepoint, RollbackToSavepoint, CommitTransaction, RollbackTransaction),
    openTransactionEvent,
    rollbackTransactionEvent,
    transactionSuccessEvent,
    HasOrvilleState (askOrvilleState, localOrvilleState),
    ConnectionState (NotConnected, Connected),
    ConnectedState (ConnectedState, connectedConnection, connectedTransaction),
    connectState,
    TransactionState (OutermostTransaction, SavepointTransaction),
    newTransaction,
    Savepoint,
    savepointNestingLevel,
    initialSavepoint,
    nextSavepoint,
  )
where

import Control.Monad.Trans.Reader (ReaderT, ask, local)
import Data.Pool (Pool)

import Orville.PostgreSQL.Connection (Connection)
import Orville.PostgreSQL.Internal.ErrorDetailLevel (ErrorDetailLevel)

{- |
  'OrvilleState' is used to manange opening connections to the database,
  transactions, etc. 'newOrvilleState' should be used to create an appopriate
  initial state for your monad's context.
-}
data OrvilleState = OrvilleState
  { _orvilleConnectionPool :: Pool Connection
  , _orvilleConnectionState :: ConnectionState
  , _orvilleErrorDetailLevel :: ErrorDetailLevel
  , _orvilleTransactionCallback :: TransactionEvent -> IO ()
  }

orvilleConnectionPool :: OrvilleState -> Pool Connection
orvilleConnectionPool =
  _orvilleConnectionPool

orvilleConnectionState :: OrvilleState -> ConnectionState
orvilleConnectionState =
  _orvilleConnectionState

orvilleErrorDetailLevel :: OrvilleState -> ErrorDetailLevel
orvilleErrorDetailLevel =
  _orvilleErrorDetailLevel

orvilleTransactionCallback :: OrvilleState -> TransactionEvent -> IO ()
orvilleTransactionCallback =
  _orvilleTransactionCallback

{- |
  Registers a callback to be invoked during transactions.

  The callback given will be called after the SQL statement corresponding
  to the given event has finished executing. Callbacks will be called
  in the order the are added.
-}
addTransactionCallback ::
  (TransactionEvent -> IO ()) ->
  OrvilleState ->
  OrvilleState
addTransactionCallback newCallback state =
  let originalCallback =
        _orvilleTransactionCallback state

      wrappedCallback event = do
        originalCallback event
        newCallback event
   in state {_orvilleTransactionCallback = wrappedCallback}

{- |
  'HasOrvilleState' is the typeclass that Orville uses to access and manange
  the connection pool and state tracking when it is being executed inside an
  unknown Monad. It is a specialized version of the Reader interface so that it
  can easily implemented by application Monads that already have a Reader
  context and want to simply add 'OrvilleState' as an attribute to that
  context, like so

  @
    data MyApplicationState =
      MyApplicationState
        { appConfig :: MyAppConfig
        , appOrvilleState :: OrvilleState
        }

    newtype MyApplicationMonad a =
      MyApplicationMonad (ReaderT MyApplicationState IO) a

    instance HasOrvilleState MyApplicationMonad where
      askOrvilleState =
        MyApplicationMonad (asks appOrvilleState)

      localOrvilleState f (MyApplicationMonad reader) =
        MyApplicationMonad $
          local
            (\state -> state { appOrvilleState = f (appOrvilleState state))
            reader
  @

  An instance for 'ReaderT OrvilleState m' is provided as a convenience in
  the case that your application has no extra context to track.
-}
class HasOrvilleState m where
  {-
    Fetches the current 'OrvilleState' from the host Monad context. The
    equivalent of 'ask' for 'ReaderT OrvilleState'
  -}
  askOrvilleState :: m OrvilleState

  {-
    Applies a modification to the 'OrvilleState' that is local to the given
    monad operation. Calls to 'askOrvilleState' made within the 'm a' provided
    must return the modified state. The modified state must only apply to
    the given 'm a' and not persisted beyond it. The equivalent of 'local'
    for 'ReaderT OrvilleState'
  -}
  localOrvilleState ::
    -- | The function to modify the 'OrvilleState'
    (OrvilleState -> OrvilleState) ->
    -- | The monad operation to execute with the modified state
    m a ->
    m a

instance Monad m => HasOrvilleState (ReaderT OrvilleState m) where
  askOrvilleState = ask
  localOrvilleState = local

{- |
  Creates a appropriate initial 'OrvilleState' that will use the connection
  pool given to initiate connections to the database.
-}
newOrvilleState :: ErrorDetailLevel -> Pool Connection -> OrvilleState
newOrvilleState errorDetailLevel pool =
  OrvilleState
    { _orvilleConnectionPool = pool
    , _orvilleConnectionState = NotConnected
    , _orvilleErrorDetailLevel = errorDetailLevel
    , _orvilleTransactionCallback = defaultTransactionCallback
    }

{- |
  Creates a new initial 'OrvilleState' using the connection pool from the
  provide state. You might need to use this if you are spawning one Orville
  monad from another and they should not share the same connection and
  transaction state.
-}
resetOrvilleState :: OrvilleState -> OrvilleState
resetOrvilleState =
  newOrvilleState
    <$> _orvilleErrorDetailLevel
    <*> _orvilleConnectionPool

{- |
  INTERNAL: Transitions the 'OrvilleState' into "connected" status, storing
  the given 'Connection' as the database connection to be used to execute
  all queries. This is used by 'withConnection' to track the connection it
  retrieves from the pool.
-}
connectState :: ConnectedState -> OrvilleState -> OrvilleState
connectState connectedState state =
  state
    { _orvilleConnectionState = Connected connectedState
    }

{- |
  INTERNAL: This type is used to signal whether a database connection has
  been retrieved from the pool for the current operation or not. The
  value is tracked in the 'OrvilleState' for the host monad, and is checked
  by 'withConnection' to avoid checking out two separate connections for a
  multiple operations that needs to be run on the same connection (e.g.
  multiple operations inside a transaction).
-}
data ConnectionState
  = NotConnected
  | Connected ConnectedState

data ConnectedState = ConnectedState
  { connectedConnection :: Connection
  , connectedTransaction :: Maybe TransactionState
  }

data TransactionState
  = OutermostTransaction
  | SavepointTransaction Savepoint

newTransaction :: Maybe TransactionState -> TransactionState
newTransaction maybeTransactionState =
  case maybeTransactionState of
    Nothing ->
      OutermostTransaction
    Just OutermostTransaction ->
      SavepointTransaction initialSavepoint
    Just (SavepointTransaction savepoint) ->
      SavepointTransaction (nextSavepoint savepoint)

{- |
  A internal Orville identifier for a savepoint in a PostgreSQL transaction.
-}
newtype Savepoint
  = Savepoint Int
  deriving (Eq, Show)

initialSavepoint :: Savepoint
initialSavepoint =
  Savepoint 1

nextSavepoint :: Savepoint -> Savepoint
nextSavepoint (Savepoint n) =
  Savepoint (n + 1)

savepointNestingLevel :: Savepoint -> Int
savepointNestingLevel (Savepoint n) = n

{- |
  Describes an event in the lifecycle of a database transaction. You can use
  'addTransactionCallBack' to register a callback to respond to these events.
  The callback will be called after the even in question has been succesfully
  executed.
-}
data TransactionEvent
  = -- | Indicates a new transaction has been started
    BeginTransaction
  | -- | Indicates that a new savepoint has been saved within a transaction
    NewSavepoint Savepoint
  | -- | Indicates that a previous savepoint has been released. It can no
    -- longer be rolled back to.
    ReleaseSavepoint Savepoint
  | -- | Indicates that rollbac was performed to a prior savepoint.
    --
    -- Note: It is possible to rollback to a savepoint prior to the most recent
    -- one without releasing or rolling back to intermediate savepoints. Doing
    -- so destroys any savepoints created after given savepoint. Although
    -- Orville currently always matches 'NewSavepoint' with either
    -- 'ReleaseSavepoint' or 'RollbackToSavepoint', it is recommended that you
    -- do not rely on this behavior.
    RollbackToSavepoint Savepoint
  | -- | Indicates that the transaction has been committed.
    CommitTransaction
  | -- | Indicates that the transaction has been rolled back.
    RollbackTransaction
  deriving (Eq, Show)

defaultTransactionCallback :: TransactionEvent -> IO ()
defaultTransactionCallback = const (pure ())

openTransactionEvent :: TransactionState -> TransactionEvent
openTransactionEvent txnState =
  case txnState of
    OutermostTransaction -> BeginTransaction
    SavepointTransaction savepoint -> NewSavepoint savepoint

rollbackTransactionEvent :: TransactionState -> TransactionEvent
rollbackTransactionEvent txnState =
  case txnState of
    OutermostTransaction -> RollbackTransaction
    SavepointTransaction savepoint -> RollbackToSavepoint savepoint

transactionSuccessEvent :: TransactionState -> TransactionEvent
transactionSuccessEvent txnState =
  case txnState of
    OutermostTransaction -> CommitTransaction
    SavepointTransaction savepoint -> ReleaseSavepoint savepoint
