{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.Bracket
  ( bracketWithResult
  , BracketResult (BracketSuccess, BracketError)
  ) where

import Control.Exception (SomeException, catch, mask, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Orville.PostgreSQL.Monad.MonadOrville (MonadOrvilleControl (liftCatch, liftMask))

data BracketResult
  = BracketSuccess
  | BracketError

{- |
  INTERNAL: A version of 'Control.Exception.bracket' that allows us to distinguish between
  exception and non-exception release cases. This is available in certain
  packages as a typeclass function under the name "generalBracket", but is
  implemented here directly in terms of IO's 'mask' and 'catch' to guarantee
  our exception handling semantics without forcing the Orville user's choice of
  library for lifting and unlift IO actions (e.g. UnliftIO).

@since 1.0.0.0
-}
bracketWithResult ::
  (MonadIO m, MonadOrvilleControl m) =>
  m a ->
  (a -> BracketResult -> m c) ->
  (a -> m b) ->
  m b
bracketWithResult acquire release action = do
  liftMask mask $ \restore -> do
    resource <- acquire

    result <-
      liftCatch
        catch
        (restore (action resource))
        (handleAndRethrow (release resource BracketError))

    _ <- release resource BracketSuccess

    pure result

{- |
  INTERNAL: Catch any exception, run the given handler, and rethrow the
  exception. This is mostly useful to force the exception being caught to be of
  the type 'SomeException'.

@since 1.0.0.0
-}
handleAndRethrow ::
  MonadIO m =>
  m a ->
  SomeException ->
  m b
handleAndRethrow handle ex = do
  _ <- handle
  liftIO . throwIO $ ex
