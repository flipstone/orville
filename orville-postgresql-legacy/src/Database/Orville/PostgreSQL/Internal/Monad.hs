{-|
Module    : Database.Orville.PostgreSQL.Internal.Monad
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.PostgreSQL.Internal.Monad where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Base
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT(..), ask, local, mapReaderT, runReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.Pool
import Database.HDBC hiding (withTransaction)

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif

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
  Migration Guide: @OrvilleEnv@ has been renamed to @OrvilleState@. It no
  longer has any type paremeters. The connection type is fixed and cannot be
  changed.

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

{- |
  Migration Guide: @setStartTransactionSQL@ has been renamed to @setBeginTransactionExpr@
-}
setStartTransactionSQL :: String -> OrvilleEnv conn -> OrvilleEnv conn
setStartTransactionSQL sql env = env {ormEnvStartTransactionSQL = sql}

defaultRunningQuery :: QueryType -> String -> IO a -> IO a
defaultRunningQuery _ _ action = action

defaultTransactionCallback :: TransactionEvent -> IO ()
defaultTransactionCallback = const (pure ())

{- |
  Migration Guide: @aroundRunningQuery@ has been renamed to @addSqlExecutionCallback@
-}
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

{- |
  Migration Guide: @addTransactionCallBack@ retains the same name
-}
addTransactionCallBack ::
     (TransactionEvent -> IO ()) -> OrvilleEnv conn -> OrvilleEnv conn
addTransactionCallBack callback env =
  env {ormEnvTransactionCallback = wrappedCallback}
  where
    wrappedCallback event = do
      ormEnvTransactionCallback env event
      callback event

{-|
  Migration Guide: @newOrvilleEnv@ has been renamed to @newOrvilleState@. The
  new function requires a parameter to be passed before the connection pool to
  specify the level of detail to be used when Orville reports errors.

 'newOrvilleEnv' initialized an 'OrvilleEnv' for service. The connection
 pool provided will be used to obtain connections to the database ase
 required. You can use the 'Database.Orville.PostgreSQL.Connection.createConnectionPool'
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

{- |
  Migration Guide: @OrvilleT@ has been removed. In its place you can simply use
  a @ReaderT OrvilleState@. If you have another @ReaderT@ layer in your monad
  stack you can add the @OrvilleState@ to the reader context for that layer
  instead, which is more efficient than having multiple @ReaderT@ layers. If
  you have a simple case of @OrvilleT conn IO@ the new Orville offers a simpler
  @Orville@ monad (not a transformer) to get you started.
-}
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
#if MIN_VERSION_base (4,11,0)
             , MonadFail
#endif
             )

{- |
  Migration Guide: @mapOrvilleT@ has been removed because @OrvilleT@ has been
  removed. If you're replacing @OrvilleT@ with @ReaderT@ then @mapOrvilleT@
  should be replaced with @mapReaderT@.
-}
mapOrvilleT ::
     Monad n => (m a -> n b) -> OrvilleT conn m a -> OrvilleT conn n b
mapOrvilleT f (OrvilleT action) = OrvilleT $ mapReaderT f action

{- |
  Migration Guide: @runOrville@ now operates on the concrete @Orville@ monad
  becase @OrvilleT@ has been removed. Assuming you are replacing usages of
  @OrvilleT@ with @ReaderT@ you will want to replace usages of @runOrville@
  with @runReaderT@.
-}
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
  Migration Guide: @HasOrvilleContext@ has been renamed to @HasOrvilleState@.
  @getOrvilleEnv@ and @localOrvilleEnv@ have been renamed to @askOrvilleState@
  and @localOrvilleState@.

  'HasOrvilleContext' defines the operations that must be available in your own
  monad for managing the connection pool that Orville functions will use to
  access the database and manage transaction state. In most cases you can
  include 'OrvilleT' in your Monad stack and then automatically derive an
  instance of 'HasOrvilleContext'.

  You could also provide your own implementations of these functions
  instead of using 'OrvilleT', if that is the easiest approach for
  your Monad.
 -}
class IConnection conn =>
      HasOrvilleContext conn m
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
   Migration Guide: @MonadOrvilleControl@ retains the same name. The
   @liftFinally@ member has been removed. There are new @liftCatch@ and
   @liftMask@ members that must be implemented, however. Instances of the new
   @MonadOrvilleControl@ are provided for @IO@ and @ReaderT@. Helper functions
   for implmenting the members via @UnliftIO@ can be found in
   @Orville.PostgreSQL.UnliftIO@.

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

   If you are using transformers in your monad stack beyond 'ReaderT', they
   probably don't provide 'MonadOrvilleControl' instances (e.g. third party
   libraries). In this case, see 'Database.Orville.PostgreSQL.MonadUnliftIO' for more
   help. If you're still stuck (because your library doesn't support
   'MonadTransControl'), try 'Database.Orville.PostgreSQL.MonadBaseControl' instead. If
   you're *still* stuck after that, please file an issue on Github at
   https://github.com/flipstone/orville so we can can help out!
  -}
class MonadOrvilleControl m where
  liftWithConnection ::
       (forall a. (conn -> IO a) -> IO a) -> (conn -> m b) -> m b
  liftFinally :: (forall a b. IO a -> IO b -> IO a) -> m c -> m d -> m c

{-|
  Migration Guide: @MonadOrville@ retains the same name, but the @conn@
  parameter has been removed. @MonadFail@ and @MonadThrow@ have been removed as
  superclass constraints.

  'MonadOrville' does not have any methods of its own. Instead it brings all
  the typeclass constraints required by Orville functions that need to access
  the database into a single typeclass. In some cases you can include
  'OrvilleT' in your Monad stack and then automatically derive an instance of
  'MonadOrville'. However, more likely you are using some third party monad
  somewhere in your stack that does not han a 'MonadOrvilleControl' instance.
  In this case you won't be able to derive 'MonadOrville', but providing a
  simple empty instance will do:

  @
    instance O.MonadOrville Postgres.Connection MyMonad
  @
 -}
class ( Monad m
      , MonadIO m
      , HasOrvilleContext conn m
      , MonadThrow m
      , MonadOrvilleControl m
#if MIN_VERSION_base(4,11,0)
      , MonadFail m
#endif
      ) =>
      MonadOrville conn m


instance MonadOrvilleControl IO where
  liftWithConnection ioWithConn = ioWithConn
  liftFinally ioFinally = ioFinally

{-|
   Migration Guide: @defaultLiftWithConnection@ has been removed. In its
   place you can use either the @ReaderT@ instance of @MonadOrvilleControl@
   or the helpers in @Orville.PostgreSQL.UnliftIO@.

   defaultLiftWithConnection provides a simple definition of
   'liftWithConnection' for 'MonadOrvilleControl' instances when the Monad in
   question is a wrapper around a type that already implements
   'MonadOrvilleControl'
  -}
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
   Migration Guide: @defaultLiftWithConnection@ has been removed (along with
   @liftFinally@)

   defaultLiftFinally provides a simple definition of
   'liftWithConnection' for 'MonadOrvilleControl' instances when the Monad in
   question is a wrapper around a type that already implements
   'MonadOrvilleControl'
  -}
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

instance (Monad m, HasOrvilleContext conn m) =>
         HasOrvilleContext conn (ReaderT a m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv modEnv = mapReaderT (localOrvilleEnv modEnv)

-- ReaderT is trivial enough that we just provide a 'MonadOrvilleControl'
-- instance for it here rather than relying on either MonadUnliftIO or
-- MonadBaseControl at all. This allows Monad stacks that only use 'ReaderT'
-- and 'OrvilleT' over IO to be built without needing to know anything more
-- about lifting IO operations beyond the types in this module.
instance MonadOrvilleControl m => MonadOrvilleControl (ReaderT a m) where
  liftWithConnection ioWithConn action = do
    ReaderT $ \env ->
      liftWithConnection ioWithConn (flip runReaderT env . action)
  liftFinally ioFinally action cleanup = do
    ReaderT $ \env ->
      liftFinally ioFinally (runReaderT action env) (runReaderT cleanup env)

instance ( Monad m
         , MonadThrow m
         , MonadIO m
         , IConnection conn
         , MonadOrville conn m
         ) =>
         MonadOrville conn (ReaderT a m)

instance MonadOrvilleControl m => MonadOrvilleControl (OrvilleT conn m) where
  liftWithConnection = defaultLiftWithConnection OrvilleT unOrvilleT
  liftFinally = defaultLiftFinally OrvilleT unOrvilleT

instance (IConnection conn, Monad m) =>
         HasOrvilleContext conn (OrvilleT conn m) where
  getOrvilleEnv = OrvilleT ask
  localOrvilleEnv modEnv (OrvilleT a) = OrvilleT (local modEnv a)

instance ( Monad m
         , MonadThrow m
         , MonadIO m
         , IConnection conn
         , MonadOrvilleControl m
#if MIN_VERSION_base (4,11,0)
         , MonadFail m
#endif
         ) =>
         MonadOrville conn (OrvilleT conn m)

-- We can provide 'HasOrvilleContext' for 'StateT' here, but not 'MonadOrvilleControl'
-- because we do not want to force a decision on the end use about how the 'StateT'
-- state should be managed during control functions (e.g. 'liftFinally'). See the
-- 'MonadBaseControl' module for an instance of 'MonadOrvilleControl' for 'StateT', if
-- you are brave enough to use 'MonadBaseControl'.
instance (Monad m, HasOrvilleContext conn m) =>
         HasOrvilleContext conn (StateT s m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv modEnv = mapStateT (localOrvilleEnv modEnv)
