{-# LANGUAGE FlexibleInstances #-}

module Orville.PostgreSQL.Internal.OrvilleState
  ( OrvilleState,
    newOrvilleState,
    resetOrvilleState,
    orvilleConnectionPool,
    orvilleConnectionState,
    orvilleErrorDetailLevel,
    HasOrvilleState (askOrvilleState, localOrvilleState),
    ConnectionState (NotConnected, Connected),
    ConnectedState (ConnectedState, connectedConnection, connectedTransaction),
    connectState,
    TransactionState (OutermostTransaction, SavepointTransaction),
    newTransaction,
    Savepoint,
    savepointNestingLevel,
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

newtype Savepoint
  = Savepoint Int

initialSavepoint :: Savepoint
initialSavepoint =
  Savepoint 1

nextSavepoint :: Savepoint -> Savepoint
nextSavepoint (Savepoint n) =
  Savepoint (n + 1)

savepointNestingLevel :: Savepoint -> Int
savepointNestingLevel (Savepoint n) = n
