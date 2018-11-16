{-# LANGUAGE RankNTypes #-}

{-|
   'Database.Orville.MonadUnliftIO provides functions and instances for using
   'MonadOrville' and 'OrvilleT' for Monad transformer stacks that are using
   'MonadUnliftIO'.  The most common way to do this is simply to add the
   following 'MonadOrvilleControl' instance:

   @
    instance MonadOrvilleControl MyMonad where
      liftWithConnection = liftWithConnectionViaUnliftIO
      liftFinally = liftFinallyViaUnliftIO
   @

   This module also provides a 'MonadUnliftIO' instance for 'OrvilleT' and 'OrvilleTrigger'.
 |-}
module Database.Orville.MonadUnliftIO
  ( liftWithConnectionViaUnliftIO
  , liftFinallyViaUnliftIO
  ) where

import qualified Control.Monad.IO.Unlift as UL

import qualified Database.Orville as O
import qualified Database.Orville.Internal.Monad as InternalMonad
import qualified Database.Orville.Internal.Trigger as InternalTrigger
import qualified Database.Orville.Trigger as OT

{-|
   liftWithConnectionViaUnliftIO can be use as the implementation of
   'liftWithConnection' for 'MonadOrvilleControl' when the 'Monad'
   implements 'MonadUnliftIO'.
 |-}
liftWithConnectionViaUnliftIO ::
     UL.MonadUnliftIO m
  => (forall a. (conn -> IO a) -> IO a)
  -> (conn -> m b)
  -> m b
liftWithConnectionViaUnliftIO ioWithConn action =
  UL.withRunInIO $ \runInIO -> ioWithConn (runInIO . action)

{-|
   liftFinallyViaUnliftIO can be use as the implementation of
   'liftFinally' for 'MonadOrvilleControl' when the 'Monad'
   implements 'MonadUnliftIO'.
 |-}
liftFinallyViaUnliftIO ::
     UL.MonadUnliftIO m
  => (forall a b. IO a -> IO b -> IO a)
  -> m c
  -> m d
  -> m c
liftFinallyViaUnliftIO ioFinally action cleanup = do
  unlio <- UL.askUnliftIO
  UL.liftIO $ ioFinally (UL.unliftIO unlio action) (UL.unliftIO unlio cleanup)

instance UL.MonadUnliftIO m => UL.MonadUnliftIO (O.OrvilleT conn m) where
  askUnliftIO =
    InternalMonad.OrvilleT $ do
      unlio <- UL.askUnliftIO
      pure $ UL.UnliftIO (UL.unliftIO unlio . InternalMonad.unOrvilleT)

instance UL.MonadUnliftIO m =>
         UL.MonadUnliftIO (OT.OrvilleTriggerT trigger conn m) where
  askUnliftIO =
    InternalTrigger.OrvilleTriggerT $ do
      unlio <- UL.askUnliftIO
      pure $ UL.UnliftIO (UL.unliftIO unlio . InternalTrigger.unTriggerT)
