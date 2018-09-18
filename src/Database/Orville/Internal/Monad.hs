{-|
Module    : Database.Orville.Internal.Monad
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.Internal.Monad where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Pool
import Database.HDBC hiding (withTransaction)

type Orville a
   = forall m conn. (MonadOrville conn m, MonadThrow m) =>
                      m a

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
  { ormEnvPool :: Pool conn
  , ormEnvConnectionEnv :: Maybe (ConnectionEnv conn)
  , ormEnvStartTransactionSQL :: String
  }

defaultStartTransactionSQL :: String
defaultStartTransactionSQL = "START TRANSACTION"

setStartTransactionSQL :: String -> OrvilleEnv conn -> OrvilleEnv conn
setStartTransactionSQL sql env = env {ormEnvStartTransactionSQL = sql}

{-|
 'newOrvilleEnv' initialized an 'OrvilleEnv' for service. The connection
 pool provided will be used to obtain connections to the database ase
 required. You can use the 'Database.Orville.PostgresSQL.createConnectionPool'
 utility function to create a connection pool to a PosgreSQL server.
-}
newOrvilleEnv :: Pool conn -> OrvilleEnv conn
newOrvilleEnv pool = OrvilleEnv pool Nothing defaultStartTransactionSQL

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
      withResource (ormEnvPool ormEnv) $ \conn -> do
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

class (Monad m, MonadIO m, IConnection conn, MonadBaseControl IO m) =>
      MonadOrville conn m
  | m -> conn
  where
  getOrvilleEnv :: m (OrvilleEnv conn)
  localOrvilleEnv :: (OrvilleEnv conn -> OrvilleEnv conn) -> m a -> m a
  runningQuery :: QueryType -> String -> m a -> m a
  runningQuery _ _ action = action

startTransactionSQL :: MonadOrville conn m => m String
startTransactionSQL = ormEnvStartTransactionSQL <$> getOrvilleEnv

instance (Monad m, MonadIO m, IConnection conn, MonadBaseControl IO m) =>
         MonadOrville conn (OrvilleT conn m) where
  getOrvilleEnv = OrvilleT ask
  localOrvilleEnv modEnv (OrvilleT a) = OrvilleT (local modEnv a)

instance MonadOrville conn m => MonadOrville conn (ReaderT a m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv modEnv action =
    ReaderT $ \val -> localOrvilleEnv modEnv (runReaderT action val)

instance MonadOrville conn m => MonadOrville conn (StateT a m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv modEnv action =
    StateT $ \val -> localOrvilleEnv modEnv (runStateT action val)

instance MonadTransControl (OrvilleT conn) where
  type StT (OrvilleT conn) a = StT (ReaderT (OrvilleEnv conn)) a
  liftWith = defaultLiftWith OrvilleT unOrvilleT
  restoreT = defaultRestoreT OrvilleT

instance MonadBaseControl b m => MonadBaseControl b (OrvilleT conn m) where
  type StM (OrvilleT conn m) a = ComposeSt (OrvilleT conn) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM
