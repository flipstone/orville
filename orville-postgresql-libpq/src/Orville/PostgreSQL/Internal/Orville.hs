{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Orville
  ( Orville,
    runOrville,
    runOrvilleWithState,
  )
where

import qualified Control.Exception.Safe as ExSafe
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool)

import Orville.PostgreSQL.Connection (Connection)
import qualified Orville.PostgreSQL.Internal.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState

{- |
  The 'Orville' Monad provides a easy starter implementation of 'MonadOrville'
  when you don't have a monad specific to your application that you need to
  use.

  If you want add Orville capabilities to your own monad, take a look at
  'MonadOrville' to learn what needs to be done.
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
    , OrvilleState.HasOrvilleState
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
-}
runOrvilleWithState :: OrvilleState.OrvilleState -> Orville a -> IO a
runOrvilleWithState state orville =
  runReaderT (unwrapOrville orville) state
