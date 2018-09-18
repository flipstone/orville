{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.Trigger
  ( insertTriggered
  , InsertTrigger(insertTriggers)
  , updateTriggered
  , UpdateTrigger(updateTriggers)
  , deleteTriggered
  , DeleteTrigger(deleteTriggers)
  , MonadTrigger(runTriggers)
  , TriggerT
  , runTriggerT
  ) where

import Control.Monad (void)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Database.Orville as O

class InsertTrigger trigger readEntity where
  insertTriggers :: readEntity -> [trigger]

class UpdateTrigger trigger readEntity writeEntity where
  updateTriggers :: readEntity -> writeEntity -> [trigger]

class DeleteTrigger trigger readEntity where
  deleteTriggers :: readEntity -> [trigger]

class MonadTrigger trigger m | m -> trigger where
  runTriggers :: [trigger] -> m ()

insertTriggered ::
     ( MonadThrow m
     , O.MonadOrville conn m
     , MonadTrigger trigger m
     , InsertTrigger trigger readEntity
     )
  => O.TableDefinition readEntity writeEntity key
  -> writeEntity
  -> m readEntity
insertTriggered tableDef writeEntity = do
  readEntity <- O.insertRecord tableDef writeEntity
  runTriggers $ insertTriggers readEntity
  pure readEntity

updateTriggered ::
     ( MonadThrow m
     , O.MonadOrville conn m
     , MonadTrigger trigger m
     , UpdateTrigger trigger readEntity writeEntity
     )
  => O.TableDefinition readEntity writeEntity key
  -> readEntity
  -> writeEntity
  -> m ()
updateTriggered tableDef oldEntity newEntity = do
  O.updateRecord tableDef (O.tableGetKey tableDef oldEntity) newEntity
  runTriggers $ updateTriggers oldEntity newEntity

deleteTriggered ::
     ( MonadThrow m
     , O.MonadOrville conn m
     , MonadTrigger trigger m
     , DeleteTrigger trigger readEntity
     )
  => O.TableDefinition readEntity writeEntity key
  -> readEntity
  -> m ()
deleteTriggered tableDef readEntity = do
  O.deleteRecord tableDef (O.tableGetKey tableDef readEntity)
  runTriggers $ deleteTriggers readEntity

data TriggerTEnv trigger m = TriggerTEnv
  { triggerTEnvAction :: TriggerAction trigger m
  , triggerTEnvTxnTriggers :: IORef (Maybe [trigger])
  }

type TriggerAction trigger m = [trigger] -> m ()

newtype TriggerT trigger m a = TriggerT
  { unTriggerT :: ReaderT (TriggerTEnv trigger m) m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase b
             , MonadThrow
             , MonadCatch
             )

instance MonadTrans (TriggerT trigger) where
  lift = TriggerT . lift

--
-- This does not follow the usual pattern of defining a MonadTransControl instance for TriggerT
-- because the Reader environment references the type `m`. This makes it impossible for TriggerT
-- to provide an implementation of `liftWith` because to do so requires producing a run function of
-- type `forall n b. Monad n => (TriggerT trigger) n b -> n b`. TriggerT can only provide such
-- a function for the type `m` that is referenced in the ReaderT context, not `forall n.` as it
-- demanded by the signature of `liftWith`.
--
instance MonadBaseControl b m => MonadBaseControl b (TriggerT trigger m) where
  type StM (TriggerT trigger m) a = StM (ReaderT (TriggerAction trigger m) m) a
  liftBaseWith action =
    TriggerT $ liftBaseWith $ \runInBase -> action (runInBase . unTriggerT)
  restoreM = TriggerT . restoreM

instance O.MonadOrville conn m =>
         O.MonadOrville conn (TriggerT trigger m) where
  getOrvilleEnv = lift O.getOrvilleEnv
  localOrvilleEnv f = TriggerT . mapReaderT (O.localOrvilleEnv f) . unTriggerT

instance MonadIO m => MonadTrigger trigger (TriggerT trigger m) where
  runTriggers triggers =
    TriggerT $ do
      triggerTEnv <- ask
      txnTriggers <- liftIO $ readIORef $ triggerTEnvTxnTriggers triggerTEnv
      case txnTriggers of
        Nothing -> lift $ triggerTEnvAction triggerTEnv triggers
        Just previousTriggers ->
          liftIO $
          writeIORef
            (triggerTEnvTxnTriggers triggerTEnv)
            (Just (previousTriggers ++ triggers))

runTriggerT ::
     O.MonadOrville conn m
  => TriggerT trigger m a
  -> TriggerAction trigger m
  -> m a
runTriggerT triggerT triggerAction = do
  txnTriggersRef <- liftIO $ newIORef Nothing -- There may be a transaction already open when runTriggerT is called!!!
  runInIO <- liftBaseWith (\run -> pure (void . run)) -- Note this runInIO discards the monadic state returned by run!!!!
  let env = TriggerTEnv triggerAction txnTriggersRef
  O.localOrvilleEnv (O.addTransactionCallBack $ trackTransactions env runInIO) $
    runReaderT (unTriggerT triggerT) env

trackTransactions ::
     TriggerTEnv trigger m -> (m () -> IO ()) -> O.TransactionEvent -> IO ()
trackTransactions env runInIO event =
  case event of
    O.TransactionStart -> writeIORef (triggerTEnvTxnTriggers env) (Just [])
    O.TransactionCommit -> do
      triggers <-
        atomicModifyIORef' (triggerTEnvTxnTriggers env) $ \triggers ->
          (Nothing, triggers)
      case triggers of
        Nothing -> pure ()
        Just t -> runInIO $ triggerTEnvAction env t
    O.TransactionRollback -> writeIORef (triggerTEnvTxnTriggers env) Nothing
