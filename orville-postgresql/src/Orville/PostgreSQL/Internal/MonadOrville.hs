{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.MonadOrville
  ( MonadOrville
  , MonadOrvilleControl (liftWithConnection, liftCatch, liftMask)
  , withConnection
  , withConnection_
  , withConnectedState
  )
where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), mapReaderT, runReaderT)

import Orville.PostgreSQL.Internal.OrvilleState
  ( ConnectedState (ConnectedState, connectedConnection, connectedTransaction)
  , ConnectionState (Connected, NotConnected)
  , OrvilleState
  , connectState
  , orvilleConnectionPool
  , orvilleConnectionState
  )
import Orville.PostgreSQL.Monad.HasOrvilleState (HasOrvilleState (askOrvilleState, localOrvilleState))
import Orville.PostgreSQL.Raw.Connection (Connection, withPoolConnection)

{- | 'MonadOrville' is the typeclass that most Orville operations require to
  do anything that connects to the database. 'MonadOrville' itself is empty,
  but it lists all the required typeclasses as superclass constraints so that
  it can be used instead of listing all the constraints on every function.

  If you want to be able to run Orville operations directly in your own
  application's Monad stack, a good starting place is to add

  @
    instance MonadOrville MyApplicationMonad
  @

  to your module and then let the compiler tell you what instances you
  are missing from the superclasses.

@since 1.0.0.0
-}
class
  ( HasOrvilleState m
  , MonadOrvilleControl m
  , MonadIO m
  ) =>
  MonadOrville m

{- | 'MonadOrvilleControl' presents the interface that Orville will use to lift
  low-level IO operations that cannot be lifted via
  'Control.Monad.IO.Class.liftIO' (i.e. those where the IO parameter is
  contravariant rather than covariant).

  For application monads built using only 'ReaderT' and 'IO', this can be
  trivially implemented (or derived), using the 'ReaderT' instance that is
  provided here. If your monad stack is sufficiently complicated, you may
  need to use the @unliftio@ package as a stepping stone to implementing
  'MonadOrvilleControl'. If your monad uses features that @unliftio@ cannot
  support (e.g. the State monad or continuations), then you may need to
  use @monad-control@ instead.

  See 'Orville.PostgreSQL.UnliftIO' for functions that can be used as the
  implementation of the methods below for monads that implement
  'Control.Monad.IO.Unlift.MonadUnliftIO'.

@since 1.0.0.0
-}
class MonadOrvilleControl m where
  -- | Orville will use this function to lift the acquisition of connections
  -- from the resource pool into the application monad.
  --
  -- @since 1.0.0.0
  liftWithConnection ::
    (forall a. (Connection -> IO a) -> IO a) -> (Connection -> m b) -> m b

  -- | Orville will use this function to lift exception catches into the
  -- application monad.
  --
  -- @since 1.0.0.0
  liftCatch ::
    Exception e =>
    (forall a. IO a -> (e -> IO a) -> IO a) ->
    m b ->
    (e -> m b) ->
    m b

  -- | Orville will use this function to lift 'Control.Exception.mask' calls
  -- into the application monad to guarantee resource cleanup is executed
  -- even when asynchronous exceptions are thrown.
  --
  -- @since 1.0.0.0
  liftMask ::
    (forall b. ((forall a. IO a -> IO a) -> IO b) -> IO b) ->
    ((forall a. m a -> m a) -> m c) ->
    m c

-- | @since 1.0.0.0
instance MonadOrvilleControl IO where
  liftWithConnection ioWithConn =
    ioWithConn

  liftCatch ioCatch =
    ioCatch

  liftMask ioMask =
    ioMask

-- | @since 1.0.0.0
instance MonadOrvilleControl m => MonadOrvilleControl (ReaderT state m) where
  liftWithConnection ioWithConn action = do
    ReaderT $ \env ->
      liftWithConnection ioWithConn (flip runReaderT env . action)

  liftCatch ioCatch action handler =
    ReaderT $ \env ->
      liftCatch
        ioCatch
        (runReaderT action env)
        (\e -> runReaderT (handler e) env)

  liftMask ioMask action =
    ReaderT $ \env ->
      liftMask ioMask $ \restore ->
        runReaderT (action (mapReaderT restore)) env

-- | @since 1.0.0.0
instance (MonadOrvilleControl m, MonadIO m) => MonadOrville (ReaderT OrvilleState m)

{- | 'withConnection' should be used to receive a 'Connection' handle for
  executing queries against the database from within an application monad using
  Orville.  For the "outermost" call of 'withConnection', a connection will be
  acquired from the resource pool. Additional calls to 'withConnection' that
  happen inside the 'm a' that uses the connection will return the same
  'Connection'. When the 'm a' finishes, the connection will be returned to the
  pool. If 'm a' throws an exception, the pool's exception handling will take
  effect, generally destroying the connection in case it was the source of the
  error.

@since 1.0.0.0
-}
withConnection :: MonadOrville m => (Connection -> m a) -> m a
withConnection connectedAction = do
  withConnectedState (connectedAction . connectedConnection)

{- | 'withConnection_' is a convenience version of 'withConnection' for those that
  don't need the actual connection handle. You might want to use this function
  even without using the handle because it ensures that all the Orville
  operations performed by the action passed to it occur on the same connection.
  Orville uses connection pooling, so unless you use either 'withConnection' or
  'Orville.PostgreSQL.withTransaction', each database operation may be
  performed on a different connection.

@since 1.0.0.0
-}
withConnection_ :: MonadOrville m => m a -> m a
withConnection_ =
  withConnection . const

{- | INTERNAL: This in an internal version of 'withConnection' that gives access to
  the entire 'ConnectedState' value to allow for transaction management.

@since 1.0.0.0
-}
withConnectedState :: MonadOrville m => (ConnectedState -> m a) -> m a
withConnectedState connectedAction = do
  state <- askOrvilleState

  case orvilleConnectionState state of
    Connected connectedState ->
      connectedAction connectedState
    NotConnected ->
      let
        pool = orvilleConnectionPool state
      in
        liftWithConnection (withPoolConnection pool) $ \conn ->
          let
            connectedState =
              ConnectedState
                { connectedConnection = conn
                , connectedTransaction = Nothing
                }
          in
            localOrvilleState (connectState connectedState) $
              connectedAction connectedState
