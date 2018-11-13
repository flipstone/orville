{-|
Module    : Database.Orville.Internal.Monad
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.Internal.Monad where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Pool
import Database.HDBC hiding (withTransaction)

data ConnectionEnv conn = ConnectionEnv
  { ormTransactionOpen :: Bool
  , ormConnection :: conn
  }

data QueryType
  = SelectQuery
  | InsertQuery
  | UpdateQuery
  | DeleteQuery
  | DDLQuery
  deriving (Ord, Eq, Enum, Show, Read)

{-|
 'OrvilleEnv' tracks all the environment information required for an
 'OrvilleT conn m' Monad to operate. Use 'newOrvilleEnv' to construct
 one.
-}
data OrvilleEnv conn = OrvilleEnv
  { ormEnvConnectionEnv :: Maybe (ConnectionEnv conn)
  , ormEnvStartTransactionSQL :: String
  , ormEnvRunningQuery :: forall a. QueryType -> String -> IO a -> IO a
  , ormEnvTransactionCallback :: TransactionEvent -> IO ()
  , ormEnvPool :: Pool conn
  }

data TransactionEvent
  = TransactionStart
  | TransactionCommit
  | TransactionRollback
  deriving (Ord, Eq, Enum, Show, Read)

defaultStartTransactionSQL :: String
defaultStartTransactionSQL = "START TRANSACTION"

setStartTransactionSQL :: String -> OrvilleEnv conn -> OrvilleEnv conn
setStartTransactionSQL sql env = env {ormEnvStartTransactionSQL = sql}

defaultRunningQuery :: QueryType -> String -> IO a -> IO a
defaultRunningQuery _ _ action = action

defaultTransactionCallback :: TransactionEvent -> IO ()
defaultTransactionCallback = const (pure ())

aroundRunningQuery ::
     (forall a. QueryType -> String -> IO a -> IO a)
  -> OrvilleEnv conn
  -> OrvilleEnv conn
aroundRunningQuery outside env = env {ormEnvRunningQuery = layeredAround}
  where
    layeredAround, inside :: QueryType -> String -> IO a -> IO a
    layeredAround queryType sql action =
      outside queryType sql (inside queryType sql action)
    inside = ormEnvRunningQuery env

addTransactionCallBack ::
     (TransactionEvent -> IO ()) -> OrvilleEnv conn -> OrvilleEnv conn
addTransactionCallBack callback env =
  env {ormEnvTransactionCallback = wrappedCallback}
  where
    wrappedCallback event = do
      ormEnvTransactionCallback env event
      callback event

{-|
 'newOrvilleEnv' initialized an 'OrvilleEnv' for service. The connection
 pool provided will be used to obtain connections to the database ase
 required. You can use the 'Database.Orville.PostgresSQL.createConnectionPool'
 utility function to create a connection pool to a PosgreSQL server.
-}
newOrvilleEnv :: Pool conn -> OrvilleEnv conn
newOrvilleEnv =
  OrvilleEnv
    Nothing
    defaultStartTransactionSQL
    defaultRunningQuery
    defaultTransactionCallback

setConnectionEnv :: ConnectionEnv conn -> OrvilleEnv conn -> OrvilleEnv conn
setConnectionEnv c ormEnv = ormEnv {ormEnvConnectionEnv = Just c}

newtype OrvilleT conn m a = OrvilleT
  { unOrvilleT :: ReaderT (OrvilleEnv conn) m a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             )

mapOrvilleT ::
     Monad n => (m a -> n b) -> OrvilleT conn m a -> OrvilleT conn n b
mapOrvilleT f (OrvilleT action) = OrvilleT $ mapReaderT f action

runOrville :: OrvilleT conn m a -> OrvilleEnv conn -> m a
runOrville = runReaderT . unOrvilleT

newConnectionEnv :: conn -> ConnectionEnv conn
newConnectionEnv = ConnectionEnv False

withConnectionEnv :: MonadOrville conn m => (ConnectionEnv conn -> m a) -> m a
withConnectionEnv action = do
  ormEnv <- getOrvilleEnv
  case ormEnvConnectionEnv ormEnv of
    Just connected -> action connected
    Nothing ->
      liftWithConnection (withResource (ormEnvPool ormEnv)) $ \conn -> do
        let connected = newConnectionEnv conn
        localOrvilleEnv (const $ ormEnv {ormEnvConnectionEnv = Just connected}) $
          action connected

withConnection :: MonadOrville conn m => (conn -> m a) -> m a
withConnection action = withConnectionEnv (action . ormConnection)

instance MonadTrans (OrvilleT conn) where
  lift = OrvilleT . lift

instance (MonadError e m) => MonadError e (OrvilleT conn m) where
  throwError = lift . throwError
  catchError action handler =
    OrvilleT ((unOrvilleT action) `catchError` (unOrvilleT . handler))

instance MonadBase b m => MonadBase b (OrvilleT conn m) where
  liftBase = lift . liftBase

{-|
  'MonadOrville' defines the operations that must be available in your
  own monad to use Orville functions that need access to the database.
  In most cases you can include 'OrvilleT' in your Monad stack and then
  automatically derive an instance of 'MonadOrville' where necessary.

  You could also provide your own implementations of these functions
  instead of using 'OrvilleT', if that is the easiest approach for
  your Monad.
 |-}
class ( Monad m
      , MonadIO m
      , IConnection conn
      , MonadOrvilleControl m
      , MonadThrow m
      ) =>
      MonadOrville conn m
  | m -> conn
  where
  getOrvilleEnv :: m (OrvilleEnv conn)
  -- ^ getOrvilleEnv fetches the Orville environment from the Monad context.
  -- Analogous to 'ask' from the 'Reader' monad.
  localOrvilleEnv :: (OrvilleEnv conn -> OrvilleEnv conn) -> m a -> m a -- ^ localOrvilleEnv locally modifies the Orville environment for the
  -- scope of the provided action. This allows Orville to track with
  -- a connection is acquired, open transactions, etc. Analogous to 'local'
  -- from the 'Reader' monad.

{-|
   'MonadOrvilleControl' provides an interface for the kinds of IO operations
   that Orville functions need to lift into the Monad providing the
   'MonadOrville' instance. This typeclass allows users to provide their
   own lifting strategies in case the Monad stack in question has special
   needs. If you are only using 'ReaderT' and 'OrvilleT' layers in your
   monad stack, you can probably implement this for your own Monad wrapper
   type using the provided default functions and providing functions to
   wrap and unwrapper your Monad layer:

   @
    instance MonadOrvilleControl MyMonad where
      liftWithConnection = defaultLiftWithConnection wrapMyMonad unWrapMyMonad
      liftFinally = defaultLiftFinally wrapMyMonad unWrapMyMonad
   @
 |-}
class MonadOrvilleControl m where
  liftWithConnection ::
       (forall a. (conn -> IO a) -> IO a) -> (conn -> m b) -> m b
  liftFinally :: (forall a b. IO a -> IO b -> IO a) -> m c -> m d -> m c

instance MonadOrvilleControl IO where
  liftWithConnection = id
  liftFinally = id

{-|
   defaultLiftWithConnection provides a simple definition of
   'liftWithConnection' for 'MonadOrvilleControl' instances when the Monad in
   question is a wrapper around a type that already implements
   'MonadOrvilleControl'
  |-}
defaultLiftWithConnection ::
     MonadOrvilleControl m
  => (forall a. m a -> n a)
  -> (forall a. n a -> m a)
  -> (forall a. (conn -> IO a) -> IO a)
  -> (conn -> n b)
  -> n b
defaultLiftWithConnection wrapT unWrapT ioWithConn action =
  wrapT $ liftWithConnection ioWithConn (unWrapT . action)

{-|
   defaultLiftFinally provides a simple definition of
   'liftWithConnection' for 'MonadOrvilleControl' instances when the Monad in
   question is a wrapper around a type that already implements
   'MonadOrvilleControl'
  |-}
defaultLiftFinally ::
     MonadOrvilleControl m
  => (forall a. m a -> n a)
  -> (forall a. n a -> m a)
  -> (forall a b. IO a -> IO b -> IO a)
  -> n c
  -> n d
  -> n c
defaultLiftFinally wrapT unWrapT ioFinally action cleanup =
  wrapT $ liftFinally ioFinally (unWrapT action) (unWrapT cleanup)

startTransactionSQL :: MonadOrville conn m => m String
startTransactionSQL = ormEnvStartTransactionSQL <$> getOrvilleEnv

instance ( Monad m
         , MonadIO m
         , IConnection conn
         , MonadOrvilleControl m
         , MonadThrow m
         ) =>
         MonadOrville conn (OrvilleT conn m) where
  getOrvilleEnv = OrvilleT ask
  localOrvilleEnv modEnv (OrvilleT a) = OrvilleT (local modEnv a)

instance MonadOrville conn m => MonadOrville conn (ReaderT a m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv modEnv action =
    ReaderT $ \val -> localOrvilleEnv modEnv (runReaderT action val)

-- ReaderT is trivial enough that we just provide a 'MonadOrvilleControl'
-- instance for it here rather than relying on either MonadUnliftIO or
-- MonadBaseControl at all. This allows Monad stacks that only use 'ReaderT'
-- and 'OrvilleT' over IO to be built without needing to know anything more
-- about lifting IO operations beyond the types in this module.
instance (Monad m, MonadOrvilleControl m) =>
         MonadOrvilleControl (ReaderT a m) where
  liftWithConnection ioWithConn action = do
    env <- ask
    lift $ liftWithConnection ioWithConn (flip runReaderT env . action)
  liftFinally ioFinally action cleanup = do
    env <- ask
    lift $
      liftFinally ioFinally (runReaderT action env) (runReaderT cleanup env)

instance (Monad m, MonadOrvilleControl m) =>
         MonadOrvilleControl (OrvilleT conn m) where
  liftWithConnection = defaultLiftWithConnection OrvilleT unOrvilleT
  liftFinally = defaultLiftFinally OrvilleT unOrvilleT
