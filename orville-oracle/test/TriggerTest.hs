{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TriggerTest where

import Control.Exception (Exception)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Database.HDBC.ODBC as ODBC

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import qualified Database.Orville.Oracle as O
import qualified Database.Orville.Oracle.Trigger as OT

import AppManagedEntity.Data.Virus (Virus(..), VirusName(..), bpsVirus)
import AppManagedEntity.Schema (schema, virusTable)

import qualified TestDB as TestDB

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif

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
              assertTriggersCommitted
                "Expected Insert Triggers to be fired"
                [Inserted oldVirus]
              TriggerTestMonad OT.clearTriggers
              void $ OT.updateTriggered virusTable oldVirus newVirus
              assertTriggersCommitted
                "Expected Update Triggers to be fired"
                [Updated oldVirus newVirus]
              TriggerTestMonad OT.clearTriggers
              void $ OT.deleteTriggered virusTable newVirus
              assertTriggersCommitted
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
                assertTriggersCommitted
                  "Expected No Triggers to be fired inside transaction"
                  []
              assertTriggersCommitted
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
                    throwM AbortTransaction) `catch`
                (\AbortTransaction -> pure ())
              assertTriggersCommitted
                "Expected No Triggers to be fired update transaction rollback"
                []
      , testCase "With Triggers around Aborted Transaction" $ do
          let oldVirus = bpsVirus
              newVirus =
                bpsVirus {virusName = VirusName (Text.pack "New Name")}
          run $ do
            TestDB.reset schema
            runTriggerTest $ do
              void $ OT.insertTriggered virusTable oldVirus
              O.withTransaction
                (do void $ OT.updateTriggered virusTable oldVirus newVirus
                    throwM AbortTransaction) `catch`
                (\AbortTransaction -> pure ())
              void $ OT.deleteTriggered virusTable newVirus
              assertTriggersCommitted
                "Expected triggers from inside transaction to have been rolled back"
                [Inserted oldVirus, Deleted newVirus]
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

newtype TriggerTestMonad a = TriggerTestMonad
  { unTriggerTest :: OT.OrvilleTriggerT TestTrigger ODBC.Connection IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , O.HasOrvilleContext ODBC.Connection
             , O.MonadOrville ODBC.Connection
             , OT.MonadTrigger TestTrigger
#if MIN_VERSION_base(4,11,0)
             , MonadFail
#endif
             )

--
-- Note that this tosses out the orville env that is in the TestDB.TestMonad instance
-- and just steals its conncetion pool to use with runOrvilleTriggerT. This is ok for the
-- tests here, but in the real world would result in the TriggerTestMonad using a different
-- database connection that the surrounding TestMonad.TestMonad was using.
--
runTriggerTest :: TriggerTestMonad () -> TestDB.TestMonad ()
runTriggerTest (TriggerTestMonad trigger) = do
  orvilleEnv <- O.getOrvilleEnv
  void $ liftIO $ OT.runOrvilleTriggerT trigger (O.ormEnvPool orvilleEnv)

assertTriggersCommitted :: String -> [TestTrigger] -> TriggerTestMonad ()
assertTriggersCommitted description expected = do
  actual <- OT.committedTriggers <$> TriggerTestMonad OT.askTriggers
  liftIO $ assertEqual description expected actual

instance O.MonadOrvilleControl TriggerTestMonad where
  liftWithConnection =
    O.defaultLiftWithConnection TriggerTestMonad unTriggerTest
  liftFinally = O.defaultLiftFinally TriggerTestMonad unTriggerTest

data AbortTransaction =
  AbortTransaction
  deriving (Eq, Show, Typeable)

instance Exception AbortTransaction
