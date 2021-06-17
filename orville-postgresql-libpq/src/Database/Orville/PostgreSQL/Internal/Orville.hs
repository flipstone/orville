{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Orville.PostgreSQL.Internal.Orville
  ( Orville,
    runOrville,
    runOrvilleWithState,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Pool (Pool)

import Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Internal.MonadOrville as MonadOrville

{- |
  The 'Orville' Monad provides a easy starter implementation of 'MonadOrville'
  when you don't have a monad specific to your application that you need to
  use.

  If you want add Orville capabilities to your own monad, take a look at
  'MonadOrville' to learn what needs to be done.
-}
newtype Orville a = Orville
  { unwrapOrville :: ReaderT MonadOrville.OrvilleState IO a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadOrville.MonadOrvilleControl
    , MonadOrville.HasOrvilleState
    )

instance MonadOrville.MonadOrville Orville

{- |
  Runs an 'Orville' operation in the 'IO' monad using the given connection
  pool.
-}
runOrville :: Pool Connection -> Orville a -> IO a
runOrville pool =
  runOrvilleWithState (MonadOrville.newOrvilleState pool)

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
runOrvilleWithState :: MonadOrville.OrvilleState -> Orville a -> IO a
runOrvilleWithState state orville =
  runReaderT (unwrapOrville orville) state
