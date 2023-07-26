{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2021-2023
License   : MIT
Stability : Stable

This module provides function that can be used to implement
'MonadOrvilleControl' for monads that implement 'MonadUnliftIO'. For example,

@
module MyMonad
  ( MyMonad
  ) where

import qualified Control.Monad.IO.Unlift as UnliftIO
import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.UnliftIO as OrvilleUnliftIO

newtype MyMonad =
  ...
  deriving (UnliftIO.MonadUnliftIO)

instance O.MonadOrvilleControl MyMonad where
  liftWithConnection = OrvilleUnliftIO.liftWithConnectionViaUnliftIO
  liftCatch = OrvilleUnliftIO.liftCatchViaUnliftIO
  liftMask = OrvilleUnliftIO.liftMaskViaUnliftIO
@

@since 0.10.0.0
-}
module Orville.PostgreSQL.UnliftIO
  ( liftWithConnectionViaUnliftIO
  , liftCatchViaUnliftIO
  , liftMaskViaUnliftIO
  )
where

import qualified Control.Monad.IO.Unlift as UL

{- |
  liftWithConnectionViaUnliftIO can be use as the implementation of
  'liftWithConnection' for 'MonadOrvilleControl' when the 'Monad'
  implements 'MonadUnliftIO'.

  @since 0.10.0.0
-}
liftWithConnectionViaUnliftIO ::
  UL.MonadUnliftIO m =>
  (forall a. (conn -> IO a) -> IO a) ->
  (conn -> m b) ->
  m b
liftWithConnectionViaUnliftIO ioWithConn action =
  UL.withRunInIO $ \runInIO -> ioWithConn (runInIO . action)

{- |
  liftCatchViaUnliftIO can be use as the implementation of
  'liftCatch' for 'MonadOrvilleControl' when the 'Monad'
  implements 'MonadUnliftIO'.

  @since 0.10.0.0
-}
liftCatchViaUnliftIO ::
  UL.MonadUnliftIO m =>
  (forall a. IO a -> (e -> IO a) -> IO a) ->
  m b ->
  (e -> m b) ->
  m b
liftCatchViaUnliftIO ioCatch action handler = do
  unlio <- UL.askUnliftIO
  UL.liftIO $
    ioCatch
      (UL.unliftIO unlio action)
      (\ex -> UL.unliftIO unlio (handler ex))

{- |
  liftMaskViaUnliftIO can be use as the implementation of
  'liftMask for 'MonadOrvilleControl' when the 'Monad'
  implements 'MonadUnliftIO'.

  @since 0.10.0.0
-}
liftMaskViaUnliftIO ::
  UL.MonadUnliftIO m =>
  (forall b. ((forall a. IO a -> IO a) -> IO b) -> IO b) ->
  ((forall a. m a -> m a) -> m c) ->
  m c
liftMaskViaUnliftIO ioMask action = do
  unlio <- UL.askUnliftIO
  UL.liftIO $
    ioMask $ \restore ->
      UL.unliftIO
        unlio
        (action (UL.liftIO . restore . UL.unliftIO unlio))
