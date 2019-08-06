{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestDB where

import Control.Monad (void, when)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.IO.Unlift as UL
import Control.Monad.Trans.Control (MonadBaseControl(..), StM)
import Data.Convertible (convert)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Database.HDBC as HDBC
import qualified Database.HDBC.ODBC as ODBC
import System.Environment (getEnv)

import Test.Tasty (TestTree, withResource)

import qualified Database.Orville.Oracle as O
import qualified Database.Orville.Oracle.MonadBaseControl as OMBC
import qualified Database.Orville.Oracle.MonadUnliftIO ()
import qualified Database.Orville.Oracle.Raw as ORaw

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif


type TestPool = Pool ODBC.Connection

data Trace = Trace
  { tracePred :: O.QueryType -> Bool
  , traceRef :: IORef [(O.QueryType, String)]
  }

newtype TestMonad a = TestMonad
  { runTestMonad :: O.OrvilleT ODBC.Connection IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadBase IO
             , MonadThrow
             , MonadCatch
             , O.HasOrvilleContext ODBC.Connection
             , O.MonadOrville ODBC.Connection
#if MIN_VERSION_base(4,11,0)
             , MonadFail
#endif
             )

queryTrace ::
     (O.QueryType -> Bool) -> TestMonad a -> TestMonad [(O.QueryType, String)]
queryTrace queryPred action = do
  ref <- liftIO (newIORef [])
  void $ O.localOrvilleEnv (addTraceToEnv (Trace queryPred ref)) action
  reverse <$> liftIO (readIORef ref)

addTraceToEnv :: Trace -> O.OrvilleEnv conn -> O.OrvilleEnv conn
addTraceToEnv trace =
  O.aroundRunningQuery $ \queryType sql action -> do
    when
      (tracePred trace queryType)
      (modifyIORef' (traceRef trace) ((queryType, sql) :))
    action

withTransactionEvents ::
     (TestMonad [O.TransactionEvent] -> TestMonad a) -> TestMonad a
withTransactionEvents action = do
  ref <- liftIO (newIORef [])
  O.localOrvilleEnv (addTransactionTraceToEnv ref) $ do
    action (getTransactionEvents ref)

addTransactionTraceToEnv ::
     IORef [O.TransactionEvent] -> O.OrvilleEnv conn -> O.OrvilleEnv conn
addTransactionTraceToEnv ref =
  O.addTransactionCallBack $ \event -> modifyIORef' ref (event :)

getTransactionEvents ::
     IORef [O.TransactionEvent] -> TestMonad [O.TransactionEvent]
getTransactionEvents = fmap reverse . liftIO . readIORef

-- This instance is still used by `ErrorsTest` because that test uses
-- `try` from `Control.Exception.Lifted`. Note that this is no longer
-- a Orville library problem, but rather an incidental demonstration
-- of the fact that a library user's Monad stack could use MonadBaseControl
-- even though Orville is no longer dependent on.
--
-- This will likely change to use MonadUnliftIO shortly as the work
-- to move away from MonadBaseControl continues.
instance MonadBaseControl IO TestMonad where
  type StM TestMonad a = StM (O.OrvilleT ODBC.Connection IO) a
  liftBaseWith f =
    TestMonad $ liftBaseWith $ \runInBase -> f (\(TestMonad m) -> runInBase m)
  restoreM stm = TestMonad (restoreM stm)

-- This instance is used by `ConduitTest` because runConduit requires the underlying
-- monad to be `MonadUnliftIO` since conduit 1.3
instance UL.MonadUnliftIO TestMonad where
  askUnliftIO =
    TestMonad $ do
      unlio <- UL.askUnliftIO
      pure $ UL.UnliftIO (UL.unliftIO unlio . runTestMonad)

instance O.MonadOrvilleControl TestMonad where
  liftWithConnection = OMBC.liftWithConnectionViaBaseControl
  liftFinally = OMBC.liftFinallyViaBaseControl

reset :: O.MonadOrville conn m => O.SchemaDefinition -> m ()
reset schemaDef = do
  results <- ORaw.selectSqlRows "SELECT current_user" []
  case results of
    [[("current_user", currentUser)]]
    -- I would like to use placeholders here, but postgres gives my a
    -- sql syntax error when I do :(
     -> void $ ORaw.updateSql ("DROP OWNED BY " ++ convert currentUser) []
    _ ->
      error $ "Expected single 'current_user' result row, got " ++ show results
  O.migrateSchema schemaDef

withOrvilleRun :: ((forall a. TestMonad a -> IO a) -> TestTree) -> TestTree
withOrvilleRun mkTree = withDb (\pool -> mkTree (run pool))
  where
    run :: IO TestPool -> forall a. TestMonad a -> IO a
    run getPool (TestMonad action) = do
      pool <- getPool
      O.runOrville action (O.newOrvilleEnv pool)

withDb :: (IO TestPool -> TestTree) -> TestTree
withDb = withResource acquirePool destroyAllResources

acquirePool :: IO TestPool
acquirePool = do
  connString <- getEnv "TEST_CONN_STRING"
  createPool (ODBC.connectODBC connString) HDBC.disconnect 1 60 1
