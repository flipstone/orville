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
  liftFinally = OrvilleUnliftIO.liftFinallyViaUnliftIO
  liftBracket = OrvilleUnliftIO.liftBracketViaUnLiftIO
@

@since 0.10.0.0
-}
module Orville.PostgreSQL.UnliftIO
  ( liftWithConnectionViaUnliftIO
  , liftFinallyViaUnliftIO
  , liftBracketViaUnLiftIO
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
  liftFinallyViaUnliftIO can be use as the implementation of
  'liftFinally' for 'MonadOrvilleControl' when the 'Monad'
  implements 'MonadUnliftIO'.

  @since 0.10.0.0
-}
liftFinallyViaUnliftIO ::
  UL.MonadUnliftIO m =>
  (forall a b. IO a -> IO b -> IO a) ->
  m c ->
  m d ->
  m c
liftFinallyViaUnliftIO ioFinally action cleanup = do
  unlio <- UL.askUnliftIO
  UL.liftIO $ ioFinally (UL.unliftIO unlio action) (UL.unliftIO unlio cleanup)

{- |
  liftBracketViaUnLiftIO can be use as the implementation of
  'liftBracket for 'MonadOrvilleControl' when the 'Monad'
  implements 'MonadUnliftIO'.

  @since 0.10.0.0
-}
liftBracketViaUnLiftIO ::
  UL.MonadUnliftIO m =>
  (forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c) ->
  m d ->
  (d -> m e) ->
  (d -> m f) ->
  m f
liftBracketViaUnLiftIO bracketIO setup final action = do
  unlio <- UL.askUnliftIO
  UL.liftIO $
    bracketIO
      (UL.unliftIO unlio setup)
      (UL.unliftIO unlio . final)
      (UL.unliftIO unlio . action)
