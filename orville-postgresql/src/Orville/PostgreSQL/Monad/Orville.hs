{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Monad.Orville
  ( Orville
  , runOrville
  , runOrvilleWithState
  )
where

import qualified Control.Exception.Safe as ExSafe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool)

import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Monad.HasOrvilleState as HasOrvilleState
import qualified Orville.PostgreSQL.Monad.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.OrvilleState as OrvilleState
import Orville.PostgreSQL.Raw.Connection (Connection)

{- |
  The 'Orville' Monad provides a easy starter implementation of 'MonadOrville'
  when you don't have a monad specific to your application that you need to
  use.

  If you want add Orville capabilities to your own monad, take a look at
  'MonadOrville' to learn what needs to be done.

@since 1.0.0.0
-}
newtype Orville a = Orville
  { unwrapOrville :: ReaderT OrvilleState.OrvilleState IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadOrville.MonadOrvilleControl
    , MonadOrville.MonadOrville
    , HasOrvilleState.HasOrvilleState
    , ExSafe.MonadThrow
    , ExSafe.MonadCatch
    )

{- |
  Runs an 'Orville' operation in the 'IO' monad using the given connection
  pool.

  This will run the 'Orville' operation with the 'ErrorDetailLevel' set to the
  default. If want to run with a different detail level, you can use
  'OrvilleState.newOrvilleState' to create a state with the desired detail
  level and then use 'runOrvilleWithState'.

@since 1.0.0.0
-}
runOrville :: Pool Connection -> Orville a -> IO a
runOrville =
  runOrvilleWithState
    . OrvilleState.newOrvilleState ErrorDetailLevel.defaultErrorDetailLevel

{- |
  Runs an 'Orville' operation in the 'IO' monad, starting from the provided
  'OrvilleState'.

  Caution: If you harvest an 'OrvilleState' from inside a
  'MonadOrville.MonadOrville' monad using 'MonadOrville.askOrvilleState',
  you may pick up connection tracking state that you didn't intend to. You
  may want to use 'MonadOrville.resetOrvilleState' in this situation to get
  a new initial state before passing it to 'runOrvilleWithState'.

  On the other hand, if you know that you want to pass the existing connection
  state from another monad into the 'Orville' monad, this is how you do it.

@since 1.0.0.0
-}
runOrvilleWithState :: OrvilleState.OrvilleState -> Orville a -> IO a
runOrvilleWithState state orville =
  runReaderT (unwrapOrville orville) state
