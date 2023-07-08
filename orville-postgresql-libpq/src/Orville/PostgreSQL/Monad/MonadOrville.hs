{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Orville.PostgreSQL.Monad.MonadOrville
  ( MonadOrville
  , MonadOrvilleControl (liftWithConnection, liftFinally, liftBracket)
  , withConnection
  , withConnectedState
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import Data.Pool (withResource)

import Orville.PostgreSQL.Monad.HasOrvilleState (HasOrvilleState (askOrvilleState, localOrvilleState))
import Orville.PostgreSQL.OrvilleState
  ( ConnectedState (ConnectedState, connectedConnection, connectedTransaction)
  , ConnectionState (Connected, NotConnected)
  , OrvilleState
  , connectState
  , orvilleConnectionPool
  , orvilleConnectionState
  )
import Orville.PostgreSQL.Raw.Connection (Connection)

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

  See 'Orville.PostgreSQL.UnliftIO' for functions that can be used as the
  implementation of the methods below for monads that implement
  'MonadUnliftIO'.
-}
class MonadOrvilleControl m where
  {-
    Orville will use this function to lift the acquisition of connections
    from the resource pool into the application monad.
  -}
  liftWithConnection ::
    (forall a. (Connection -> IO a) -> IO a) -> (Connection -> m b) -> m b

  {-
    Orville will use this function to resource cleanup actions from IO
    into the application monad.
  -}
  liftFinally :: (forall a b. IO a -> IO b -> IO a) -> m c -> m d -> m c

  {-
    Orville will use this function to lift resource allocation and cleanup
    from IO into the application monad.
  -}
  liftBracket ::
    (forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c) ->
    m d ->
    (d -> m e) ->
    (d -> m f) ->
    m f

instance MonadOrvilleControl IO where
  liftWithConnection ioWithConn =
    ioWithConn

  liftFinally ioFinally =
    ioFinally

  liftBracket ioBracket =
    ioBracket

instance MonadOrvilleControl m => MonadOrvilleControl (ReaderT state m) where
  liftWithConnection ioWithConn action = do
    ReaderT $ \env ->
      liftWithConnection ioWithConn (flip runReaderT env . action)

  liftFinally ioFinally action cleanup = do
    ReaderT $ \env ->
      liftFinally
        ioFinally
        (runReaderT action env)
        (runReaderT cleanup env)

  liftBracket ioBracket allocate cleanup action = do
    ReaderT $ \env ->
      liftBracket
        ioBracket
        (runReaderT allocate env)
        (\resource -> runReaderT (cleanup resource) env)
        (\resource -> runReaderT (action resource) env)

instance (MonadOrvilleControl m, MonadIO m) => MonadOrville (ReaderT OrvilleState m)

{- |
  'withConnection' should be used to receive a 'Connection' handle for
  executing queries against the database from within an application monad using
  Orville.  For the "outermost" call of 'withConnection', a connection will be
  acquired from the resource pool. Additional calls to 'withConnection' that
  happen inside the 'm a' that uses the connection will return the same
  'Connection' the same connection. When the 'm a' finishes the connection
  will be returned to the pool. If 'm a' throws an exception the pool's
  exception handling will take effect, generally destroying the connection in
  case it was the source of the error.
-}
withConnection :: MonadOrville m => (Connection -> m a) -> m a
withConnection connectedAction = do
  withConnectedState (connectedAction . connectedConnection)

{- |
  INTERNAL: This in an internal version of 'withConnection' that gives access to
  the entire 'ConnectedState' value to allow for transaction management.
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
        liftWithConnection (withResource pool) $ \conn ->
          let
            connectedState =
              ConnectedState
                { connectedConnection = conn
                , connectedTransaction = Nothing
                }
          in
            localOrvilleState (connectState connectedState) $
              connectedAction connectedState
