{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

This module provides functions that can be used to implement
'Orville.PostgreSQL.MonadOrvilleControl' for monads that implement
'UL.MonadUnliftIO'. For example:

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

@since 1.0.0.0
-}
module Orville.PostgreSQL.UnliftIO
  ( liftWithConnectionViaUnliftIO
  , liftCatchViaUnliftIO
  , liftMaskViaUnliftIO
  )
where

import qualified Control.Monad.IO.Unlift as UL

{- |
  'liftWithConnectionViaUnliftIO' can be used as the implementation of
  'Orville.PostgreSQL.liftWithConnection' for
  'Orville.PostgreSQL.MonadOrvilleControl' when the 'Monad' implements
  'UL.MonadUnliftIO'.

  @since 1.0.0.0
-}
liftWithConnectionViaUnliftIO ::
  UL.MonadUnliftIO m =>
  (forall a. (conn -> IO a) -> IO a) ->
  (conn -> m b) ->
  m b
liftWithConnectionViaUnliftIO ioWithConn action =
  UL.withRunInIO $ \runInIO -> ioWithConn (runInIO . action)

{- |
  'liftCatchViaUnliftIO' can be used as the implementation of
  'Orville.PostgreSQL.liftCatch' for 'Orville.PostgreSQL.MonadOrvilleControl'
  when the 'Monad' implements 'UL.MonadUnliftIO'.

  @since 1.0.0.0
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
  'liftMaskViaUnliftIO' can be used as the implementation of
  'Orville.PostgreSQL.liftMask' for 'Orville.PostgreSQL.MonadOrvilleControl'
  when the 'Monad' implements 'UL.MonadUnliftIO'.

  @since 1.0.0.0
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
