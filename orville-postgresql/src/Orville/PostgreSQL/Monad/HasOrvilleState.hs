{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Monad.HasOrvilleState
  ( HasOrvilleState (askOrvilleState, localOrvilleState)
  )
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local, mapReaderT)

import Orville.PostgreSQL.OrvilleState (OrvilleState)

{- |
  'HasOrvilleState' is the typeclass that Orville uses to access and manange
  the connection pool and state tracking when it is being executed inside an
  unknown Monad. It is a specialized version of the Reader interface so that it
  can easily implemented by application Monads that already have a Reader
  context and want to simply add 'OrvilleState' as an attribute to that
  context, like so

  @
    data MyApplicationState =
      MyApplicationState
        { appConfig :: MyAppConfig
        , appOrvilleState :: OrvilleState
        }

    newtype MyApplicationMonad a =
      MyApplicationMonad (ReaderT MyApplicationState IO) a

    instance HasOrvilleState MyApplicationMonad where
      askOrvilleState =
        MyApplicationMonad (asks appOrvilleState)

      localOrvilleState f (MyApplicationMonad reader) =
        MyApplicationMonad $
          local
            (\state -> state { appOrvilleState = f (appOrvilleState state))
            reader
  @

  An instance for 'ReaderT OrvilleState m' is provided as a convenience in
  the case that your application has no extra context to track.

@since 1.0.0.0
-}
class HasOrvilleState m where
  -- |
  --     Fetches the current 'OrvilleState' from the host Monad context. The
  --     equivalent of 'ask' for 'ReaderT OrvilleState'
  --
  -- @since 1.0.0.0
  askOrvilleState :: m OrvilleState

  -- |
  --     Applies a modification to the 'OrvilleState' that is local to the given
  --     monad operation. Calls to 'askOrvilleState' made within the 'm a' provided
  --     must return the modified state. The modified state must only apply to
  --     the given 'm a' and not persisted beyond it. The equivalent of 'local'
  --     for 'ReaderT OrvilleState'
  --
  -- @since 1.0.0.0
  localOrvilleState ::
    -- | The function to modify the 'OrvilleState'
    (OrvilleState -> OrvilleState) ->
    -- | The monad operation to execute with the modified state
    m a ->
    m a

instance Monad m => HasOrvilleState (ReaderT OrvilleState m) where
  askOrvilleState = ask
  localOrvilleState = local

instance {-# OVERLAPS #-} (Monad m, HasOrvilleState m) => HasOrvilleState (ReaderT r m) where
  askOrvilleState = lift askOrvilleState
  localOrvilleState f = mapReaderT (localOrvilleState f)
