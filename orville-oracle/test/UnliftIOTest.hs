{-# LANGUAGE CPP #-}
module UnliftIOTest where

import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.IO.Unlift as UL
import qualified Database.HDBC.ODBC as ODBC
import qualified Database.Orville.Oracle as O
import qualified Database.Orville.Oracle.MonadUnliftIO as OULIO

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified TestDB as TestDB

#if MIN_VERSION_base(4,11,0)
import Control.Monad.Fail (MonadFail)
#endif

{-|
   'ThirdPartyMonad' is a stand in for a Monad (or Monad Transformer) that an
   Orville user might by using from another library that doesn't know anything
   about the Orville Monad stack. We would like to make using such third
   parties painless as possible.
  -}
newtype ThirdPartyMonad a = ThirdPartyMonad
  { runThirdPartyMonad :: IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , UL.MonadIO
             , UL.MonadUnliftIO
             , MonadThrow
#if MIN_VERSION_base(4,11,0)
             , MonadFail
#endif
             )

{-|
   'EndUserMonad' is a stand in for the Monad stack that an Orville user might
   build using a third party monad from another library. We would like to make
   it easy to build the typeclass instances that Orville requires as easy as
   possible for new users.
  -}
newtype EndUserMonad a = EndUserMonad
  { runEndUserMonad :: O.OrvilleT ODBC.Connection ThirdPartyMonad a
  } deriving ( Functor
             , Applicative
             , Monad
             , UL.MonadIO
             , O.HasOrvilleContext ODBC.Connection
             , MonadThrow
#if MIN_VERSION_base(4,11,0)
             , MonadFail
#endif

             )

{-|
   This cannot be derived because 'ThirdPartyMonad' does not implement
   'MonadOrvilleControl'. Declaring an empty instance is trivial, however, and
   avoids having to provide an orphan instance of 'MonadOrvilleControl' for
   'ThirdPartyMonad'.
  -}
instance O.MonadOrville ODBC.Connection EndUserMonad

{-|
   If the user is using 'MonadUnliftIO', then it would be up to them to provide
   this instance for their own Monad. Later versions of UnliftIO provide a helper
   function for implementing this, but we would like to keep the dependency bounds
   as broad as possibly, so we can't use that helper here.
  -}
instance UL.MonadUnliftIO EndUserMonad where
  askUnliftIO =
    EndUserMonad $ do
      unlio <- UL.askUnliftIO
      pure $ UL.UnliftIO (UL.unliftIO unlio . runEndUserMonad)

{-|
   This is the 'MonadOrvilleControl' instance that a user would need to built if
   they are using 'MonadUnliftIO' as their lifting strategy. We would like this to
   be trivial enough that we could easily provide it in the documentation of a
   quick start tutorial.
  -}
instance O.MonadOrvilleControl EndUserMonad where
  liftWithConnection = OULIO.liftWithConnectionViaUnliftIO
  liftFinally = OULIO.liftFinallyViaUnliftIO

orvilleAction :: EndUserMonad ()
orvilleAction = TestDB.reset []

test_migrate :: TestTree
test_migrate =
  TestDB.withDb $ \getPool ->
    testGroup
      "UnliftIO"
      [ testCase "works" $ do
          pool <- getPool
          runThirdPartyMonad $
            O.runOrville (runEndUserMonad orvilleAction) (O.newOrvilleEnv pool)
      ]
