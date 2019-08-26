{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Orville.Oracle.Internal.Trigger where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.Trans (MonadTrans(lift))
import qualified Data.DList as DList
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , atomicWriteIORef
  , newIORef
  , readIORef
  )
import Data.Monoid ((<>))
import Data.Pool (Pool)
import qualified Database.HDBC as HDBC
import qualified Database.Orville.Oracle as O

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif

class MonadTrigger trigger m | m -> trigger where
  runTriggers :: [trigger] -> m ()

class InsertTrigger trigger readEntity where
  insertTriggers :: readEntity -> [trigger]

class UpdateTrigger trigger readEntity writeEntity where
  updateTriggers :: readEntity -> writeEntity -> [trigger]

class DeleteTrigger trigger readEntity where
  deleteTriggers :: readEntity -> [trigger]

-- insertTriggered ::
--      ( MonadThrow m
--      , O.MonadOrville conn m
--      , MonadTrigger trigger m
--      , InsertTrigger trigger readEntity
--      )
--   => O.TableDefinition readEntity writeEntity key
--   -> writeEntity
--   -> m readEntity
-- insertTriggered tableDef writeEntity = do
--   readEntity <- O.insertRecord tableDef writeEntity
--   runTriggers $ insertTriggers readEntity
--   pure readEntity

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

type RecordedTriggersRef trigger = IORef (RecordedTriggers trigger)

data RecordedTriggers trigger = RecordedTriggers
  { committedTriggersDList :: DList.DList trigger
  , uncommittedTriggersDList :: Maybe (DList.DList trigger)
  }

committedTriggers :: RecordedTriggers trigger -> [trigger]
committedTriggers = DList.toList . committedTriggersDList

uncommittedTriggers :: RecordedTriggers trigger -> Maybe [trigger]
uncommittedTriggers = fmap DList.toList . uncommittedTriggersDList

emptyTriggerData :: RecordedTriggers trigger
emptyTriggerData = RecordedTriggers mempty mempty

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (\a -> (f a, ()))

recordTriggers :: RecordedTriggersRef trigger -> [trigger] -> IO ()
recordTriggers ref triggers =
  atomicModifyIORef'_ ref $ \recorded ->
    case uncommittedTriggersDList recorded of
      Just uncommitted ->
        recorded
          { uncommittedTriggersDList =
              Just (uncommitted <> DList.fromList triggers)
          }
      Nothing ->
        recorded
          { committedTriggersDList =
              committedTriggersDList recorded <> DList.fromList triggers
          }

startTriggerTxn :: RecordedTriggersRef trigger -> IO ()
startTriggerTxn ref =
  atomicModifyIORef'_ ref $ \recorded ->
    case uncommittedTriggers recorded of
      Just _ -> recorded
      Nothing -> recorded {uncommittedTriggersDList = Just DList.empty}

commitTriggerTxn :: RecordedTriggersRef trigger -> IO ()
commitTriggerTxn ref =
  atomicModifyIORef'_ ref $ \recorded ->
    case uncommittedTriggersDList recorded of
      Just uncommitted ->
        recorded
          { uncommittedTriggersDList = Nothing
          , committedTriggersDList =
              committedTriggersDList recorded <> uncommitted
          }
      Nothing -> recorded

rollbackTriggerTxn :: RecordedTriggersRef trigger -> IO ()
rollbackTriggerTxn ref =
  atomicModifyIORef'_ ref $ \recorded ->
    recorded {uncommittedTriggersDList = Nothing}

newtype OrvilleTriggerT trigger conn m a = OrvilleTriggerT
  { unTriggerT :: ReaderT (RecordedTriggersRef trigger) (O.OrvilleT conn m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase b
             , MonadThrow
             , MonadCatch
             , MonadMask
#if MIN_VERSION_base (4,11,0)
             , MonadFail
#endif
             )

instance MonadTrans (OrvilleTriggerT trigger conn) where
  lift = OrvilleTriggerT . lift . lift

instance (MonadError e m) =>
         MonadError e (OrvilleTriggerT trigger conn m) where
  throwError = lift . throwError
  catchError action handler =
    OrvilleTriggerT ((unTriggerT action) `catchError` (unTriggerT . handler))

instance (Monad m, HDBC.IConnection conn) =>
         O.HasOrvilleContext conn (OrvilleTriggerT trigger conn m) where
  getOrvilleEnv = OrvilleTriggerT $ lift O.getOrvilleEnv
  localOrvilleEnv f =
    OrvilleTriggerT . mapReaderT (O.localOrvilleEnv f) . unTriggerT

instance MonadIO m =>
         MonadTrigger trigger (OrvilleTriggerT trigger conn m) where
  runTriggers triggers =
    OrvilleTriggerT $ do
      recordedTriggers <- ask
      liftIO $ recordTriggers recordedTriggers triggers

instance (Monad m, O.MonadOrvilleControl m) =>
         O.MonadOrvilleControl (OrvilleTriggerT trigger conn m) where
  liftWithConnection = O.defaultLiftWithConnection OrvilleTriggerT unTriggerT
  liftFinally = O.defaultLiftFinally OrvilleTriggerT unTriggerT

instance ( Monad m
         , MonadThrow m
         , MonadIO m
         , HDBC.IConnection conn
         , O.MonadOrvilleControl m
#if MIN_VERSION_base (4,11,0)
         , MonadFail m
#endif
         ) =>
         O.MonadOrville conn (OrvilleTriggerT trigger conn m)

{-
   `askTriggers` retrieves triggers that have been recorded thus far. If you
   do not want to see the triggers returned again from future calls, you should
   use `clearTriggers` as well.
 -}
askTriggers ::
     MonadIO m => OrvilleTriggerT trigger conn m (RecordedTriggers trigger)
askTriggers =
  OrvilleTriggerT $ do
    recordedTriggers <- ask
    liftIO $ readIORef recordedTriggers

{-
   `clearTriggers` clears out the trigger list. This is useful if you have
   processed the trigger list and don't want to see those triggers again.
 -}
clearTriggers :: MonadIO m => OrvilleTriggerT trigger conn m ()
clearTriggers =
  OrvilleTriggerT $ do
    recordedTriggers <- ask
    liftIO $ atomicWriteIORef recordedTriggers emptyTriggerData

{-
   `runOrvilleTriggerT` runs an Orville actions that has triggering behavior and
   returns the triggers that were committed. Note there there will never be any
   *uncommitted* triggers at the end because any `withTransaction` block must
   by contained *within* the action passed  to `runOrvilleTriggerT`. If you
   layer `OrvilleTriggerT` on top of a Monad `m` that *also* allows for database
   connection activity, god rest your soul.

   Note that if an exception occurs in `m` and is not caught within the action passed
   to `runOrvilleTriggerT`, you will lose any triggers that may have happened up to
   the point of the action, including those related to database operations that were
   successfully committed. If you wish to respond to those triggers, you need to perform
   some Exception handling inside the action given to `runOrvilleTriggerT` and use
   `askTriggers` to retrieve the triggers inside the exception handler.
 -}
runOrvilleTriggerT ::
     (MonadIO m)
  => OrvilleTriggerT trigger conn m a
  -> Pool conn
  -> m (a, [trigger])
runOrvilleTriggerT triggerT pool = do
  ref <- liftIO $ newIORef emptyTriggerData
  let orvilleT = runReaderT (unTriggerT triggerT) ref
      orvilleEnv =
        O.addTransactionCallBack (trackTransactions ref) (O.newOrvilleEnv pool)
  a <- O.runOrville orvilleT orvilleEnv
  triggers <- committedTriggers <$> liftIO (readIORef ref)
  pure (a, triggers)

trackTransactions :: RecordedTriggersRef trigger -> O.TransactionEvent -> IO ()
trackTransactions recorded event =
  case event of
    O.TransactionStart -> startTriggerTxn recorded
    O.TransactionCommit -> commitTriggerTxn recorded
    O.TransactionRollback -> rollbackTriggerTxn recorded
