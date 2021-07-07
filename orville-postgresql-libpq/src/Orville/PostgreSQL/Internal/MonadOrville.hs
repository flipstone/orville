{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Orville.PostgreSQL.Internal.MonadOrville
  ( MonadOrville,
    HasOrvilleState (askOrvilleState, localOrvilleState),
    OrvilleState (OrvilleState),
    newOrvilleState,
    resetOrvilleState,
    MonadOrvilleControl (liftWithConnection),
    withConnection,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), ask, local, runReaderT)
import Data.Pool (Pool, withResource)

import Orville.PostgreSQL.Connection (Connection)

{- |
  'MonadOrville' is the typeclass that most Orville operations require to
  do anything that connects to the database. 'MonadOrville' itself is empty,
  but it lists all the required typeclasses as superclass contraints so that
  it can be used instead of listing all the constraints on every function.

  If you want to be able to run Orville operations directly in your own
  application's Monad stack, a good starting place is to add

  @
    instance MonadOrville MyApplicationMonad
  @

  to your module and then let the compiler tell you what instances you
  are missing from the superclasses.
-}
class
  ( HasOrvilleState m
  , MonadOrvilleControl m
  , MonadIO m
  ) =>
  MonadOrville m

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
  'OrvilleState' is used to manange opening connections to the database,
  transactions, etc. 'newOrvilleState should be used to create an appopriate
  initial state for your monad's context.
-}
data OrvilleState = OrvilleState
  { orvilleConnectionPool :: Pool Connection
  , orvilleConnectionState :: ConnectionState
  }

{- |
  Creates a appropriate initial 'OrvilleState' that will use the connection
  pool given to initiate connections to the database.
-}
newOrvilleState :: Pool Connection -> OrvilleState
newOrvilleState pool =
  OrvilleState
    { orvilleConnectionPool = pool
    , orvilleConnectionState = NotConnected
    }

{- |
  Creates a new initial 'OrvilleState' using the connection pool from the
  provide state. You might need to use this if you are spawning one Orville
  monad from another and they should not share the same connection and
  transaction state.
-}
resetOrvilleState :: OrvilleState -> OrvilleState
resetOrvilleState =
  newOrvilleState . orvilleConnectionPool

{- |
  INTERNAL: Transitions the 'OrvilleState' into "connected" status, storing
  the given 'Connection' as the database connection to be used to execute
  all queries. This is used by 'withConnection' to track the connection it
  retrieves from the pool.
-}
connectState :: Connection -> OrvilleState -> OrvilleState
connectState conn context =
  context
    { orvilleConnectionState = Connected conn
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
  | Connected Connection

{- |
  'MonadOrvilleControl' presents the interface that Orville will used to
  lift low-level IO operations that cannot be lifted via 'liftIO' (i.e.
  those where the IO parameter is contravriant rather than covariant).

  For application monads built using only 'ReaderT' and 'IO', this can be
  trivially implemented (or derived), using the 'ReaderT' instance that is
  provided here. If you monad stack is sufficiently complicated, you may
  need to use the 'unliftio' package as a stepping stone to implementing
  'MonadOrvilleControl'. If your monad uses features that 'unliftio' cannot
  support (e.g. the State monad or continuations), then you may need to
  use 'monad-control' instead.

  TODO: Orville will provide helpers for using 'unliftio' and 'monad-control',
  before release, we just haven't gotten quite there yet.
-}
class MonadOrvilleControl m where
  {-
    Orville will use this function to lift the acquisition of connections
    from the resource pool into the application monad.
  -}
  liftWithConnection ::
    (forall a. (Connection -> IO a) -> IO a) -> (Connection -> m b) -> m b

instance MonadOrvilleControl IO where
  liftWithConnection ioWithConn =
    ioWithConn

instance MonadOrvilleControl m => MonadOrvilleControl (ReaderT context m) where
  liftWithConnection ioWithConn action = do
    ReaderT $ \env ->
      liftWithConnection ioWithConn (flip runReaderT env . action)

{- |
  'withConnection' should be used to receive a 'Connection' handle for
  executing queries against the database from within an application monad using
  Orville.  For the "outermost" call of 'withConnection', a connection will be
  acquired from the resource pool. Additional calls to 'withConnection' that
  happen inside the 'm a' that uses the connection with return the same
  'Connection' the same connection. When the 'm a' finishes the connection
  will be returned to the pool. If 'm a' throws an exception the pool's
  exception handling will take effect, generally destroying the connection in
  case it was the source of the error.
-}
withConnection :: MonadOrville m => (Connection -> m a) -> m a
withConnection connectedAction = do
  context <- askOrvilleState

  case orvilleConnectionState context of
    Connected conn ->
      connectedAction conn
    NotConnected -> do
      let pool = orvilleConnectionPool context
      liftWithConnection (withResource pool) $ \conn ->
        localOrvilleState (connectState conn) $
          connectedAction conn
