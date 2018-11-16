{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadBaseControlTest where

import qualified Control.Monad.Base as MB
import Control.Monad.Catch (MonadThrow)
import qualified Control.Monad.IO.Class as MIO
import qualified Control.Monad.Trans.Control as MTC
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville as O
import qualified Database.Orville.MonadBaseControl as OMBC

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import qualified TestDB as TestDB

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
             , MIO.MonadIO
             , MB.MonadBase IO
             , MonadThrow
             )

instance MTC.MonadBaseControl IO ThirdPartyMonad where
  type StM ThirdPartyMonad a = MTC.StM IO a
  liftBaseWith withRunInBase =
    ThirdPartyMonad $
    MTC.liftBaseWith $ \runInBase ->
      withRunInBase (runInBase . runThirdPartyMonad)
  restoreM = ThirdPartyMonad . MTC.restoreM

{-|
   'EndUserMonad' is a stand in for the Monad stack that an Orville user might
   build using a third party monad from another library. We would like to make
   it easy to build the typeclass instances that Orville requires as easy as
   possible for new users.
  -}
newtype EndUserMonad a = EndUserMonad
  { runEndUserMonad :: O.OrvilleT Postgres.Connection ThirdPartyMonad a
  } deriving ( Functor
             , Applicative
             , Monad
             , MIO.MonadIO
             , MB.MonadBase IO
             , O.MonadOrville Postgres.Connection
             , MonadThrow
             )

{-|
   If the user is using 'MonadBaseControl', then it would be up to them to
   provide this instance for their own Monad. It is perhaps worth noting that
   the 'defaultLiftBaseWith' and 'defaultRestoreM' from 'MonadBaseControl' are
   not particularly helpful here, because the rely on having an instance of
   'MonadTransControl', but newtype wrappers used to cap-off Monad stacks are
   not transformers, so they can provide such an instance.
  -}
instance MTC.MonadBaseControl IO EndUserMonad where
  type StM EndUserMonad a = MTC.StM (O.OrvilleT Postgres.Connection ThirdPartyMonad) a
  liftBaseWith withRunInBase =
    EndUserMonad $
    MTC.liftBaseWith $ \runInBase ->
      withRunInBase (runInBase . runEndUserMonad)
  restoreM = EndUserMonad . MTC.restoreM

{-|
   This is the 'MonadOrvilleControl' instance that a user would need to built if
   they are using 'MonadBaseControl' as their lifting strategy.
  -}
instance O.MonadOrvilleControl EndUserMonad where
  liftWithConnection = OMBC.liftWithConnectionViaBaseControl
  liftFinally = OMBC.liftFinallyViaBaseControl

{-|
   The organization of the Orville typeclasses currently requires this orphan
   instance to be provided by the user for third-party monad's they are using.
   Although it is trivial and relatively innocent, we would prefer to avoid
   requiring orphan instances when using Orville or even introducing new users
   to the concept.
  -}
instance O.MonadOrvilleControl ThirdPartyMonad where
  liftWithConnection = OMBC.liftWithConnectionViaBaseControl
  liftFinally = OMBC.liftFinallyViaBaseControl

orvilleAction :: EndUserMonad ()
orvilleAction = TestDB.reset []

test_migrate :: TestTree
test_migrate =
  TestDB.withDb $ \getPool ->
    testGroup
      "MonadBaseControl"
      [ testCase "works" $ do
          pool <- getPool
          runThirdPartyMonad $
            O.runOrville (runEndUserMonad orvilleAction) (O.newOrvilleEnv pool)
      ]
