{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TriggerTest where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Database.HDBC.PostgreSQL as Postgres

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Database.Orville as O
import qualified Database.Orville.Trigger as OT

import AppManagedEntity.Data.Virus (Virus(..), VirusName(..), bpsVirus)
import AppManagedEntity.Schema (schema, virusTable)

import qualified TestDB as TestDB

test_trigger :: TestTree
test_trigger =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Trigger Test"
      [ testCase "Without Transaction" $ do
          let oldVirus = bpsVirus
              newVirus =
                bpsVirus {virusName = VirusName (Text.pack "New Name")}
          run $ do
            TestDB.reset schema
            runTriggerTest $ do
              void $ OT.insertTriggered virusTable oldVirus
              assertTriggersReceived
                "Expected Insert Triggers to be fired"
                [Inserted oldVirus]
              clearTriggers
              void $ OT.updateTriggered virusTable oldVirus newVirus
              assertTriggersReceived
                "Expected Update Triggers to be fired"
                [Updated oldVirus newVirus]
              clearTriggers
              void $ OT.deleteTriggered virusTable newVirus
              assertTriggersReceived
                "Expected Delete Triggers to be fired"
                [Deleted newVirus]
      , testCase "With Successful Transaction" $ do
          let oldVirus = bpsVirus
              newVirus =
                bpsVirus {virusName = VirusName (Text.pack "New Name")}
          run $ do
            TestDB.reset schema
            runTriggerTest $ do
              O.withTransaction $ do
                void $ OT.insertTriggered virusTable oldVirus
                void $ OT.updateTriggered virusTable oldVirus newVirus
                void $ OT.deleteTriggered virusTable newVirus
                assertTriggersReceived
                  "Expected No Triggers to be fired inside transaction"
                  []
              assertTriggersReceived
                "Expected All Triggers to be fired update transaction commit"
                [ Inserted oldVirus
                , Updated oldVirus newVirus
                , Deleted newVirus
                ]
      , testCase "With Aborted Transaction" $ do
          let oldVirus = bpsVirus
              newVirus =
                bpsVirus {virusName = VirusName (Text.pack "New Name")}
          run $ do
            TestDB.reset schema
            runTriggerTest $ do
              O.withTransaction
                (do void $ OT.insertTriggered virusTable oldVirus
                    void $ OT.updateTriggered virusTable oldVirus newVirus
                    void $ OT.deleteTriggered virusTable newVirus
                    assertTriggersReceived
                      "Expected No Triggers to be fired inside transaction"
                      []
                    throwM AbortTransaction) `catch`
                (\AbortTransaction -> pure ())
              assertTriggersReceived
                "Expected No Triggers to be fired update transaction rollback"
                []
      ]

data TestTrigger
  = Inserted Virus
  | Updated Virus
            Virus
  | Deleted Virus
  deriving (Eq, Show)

instance OT.InsertTrigger TestTrigger Virus where
  insertTriggers virus = [Inserted virus]

instance OT.UpdateTrigger TestTrigger Virus Virus where
  updateTriggers old new = [Updated old new]

instance OT.DeleteTrigger TestTrigger Virus where
  deleteTriggers virus = [Deleted virus]

newtype TriggerTestMonad a =
  TriggerTestMonad (OT.TriggerT TestTrigger (ReaderT (IORef [TestTrigger]) TestDB.TestMonad) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadBase IO
           , MonadThrow
           , MonadCatch
           , O.MonadOrville Postgres.Connection
           , OT.MonadTrigger TestTrigger
           )

runTriggerTest :: TriggerTestMonad a -> TestDB.TestMonad a
runTriggerTest (TriggerTestMonad trigger) = do
  triggersRef <- liftIO $ newIORef []
  runReaderT (OT.runTriggerT trigger appendTriggers) triggersRef

appendTriggers ::
     [TestTrigger] -> ReaderT (IORef [TestTrigger]) TestDB.TestMonad ()
appendTriggers triggers = do
  triggersRef <- ask
  liftIO $ modifyIORef' triggersRef (++ triggers)

askTriggers :: TriggerTestMonad [TestTrigger]
askTriggers =
  TriggerTestMonad $ do
    triggersRef <- lift ask
    liftIO $ readIORef triggersRef

clearTriggers :: TriggerTestMonad ()
clearTriggers =
  TriggerTestMonad $ do
    triggersRef <- lift ask
    liftIO $ writeIORef triggersRef []

assertTriggersReceived :: String -> [TestTrigger] -> TriggerTestMonad ()
assertTriggersReceived description expected = do
  actual <- askTriggers
  liftIO $ assertEqual description expected actual

instance MonadBaseControl IO TriggerTestMonad where
  type StM TriggerTestMonad a = StM TestDB.TestMonad a
  liftBaseWith f =
    TriggerTestMonad $
    liftBaseWith $ \runInBase -> f (\(TriggerTestMonad m) -> runInBase m)
  restoreM stm = TriggerTestMonad (restoreM stm)

data AbortTransaction =
  AbortTransaction
  deriving (Eq, Show, Typeable)

instance Exception AbortTransaction
