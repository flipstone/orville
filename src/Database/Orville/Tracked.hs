{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Orville.Tracked
  ( Sign(..)
  , SignType(..)
  , signTableAs
  , signEntityFrom

  , TrackedOrville
  , Trail(..)
  , MonadTrackedOrville (..)

  , insertRecordTracked
  , updateRecordTracked

  , mapTrackedOrville
  , runTrackedOrville
  , trackedMapper
  , trailToList
  ) where

import            Control.Applicative
import            Control.Monad.Base
import            Control.Monad.Catch
import            Control.Monad.Except
import            Control.Monad.Trans.Control
import            Control.Monad.Writer
import qualified  Data.DList as D
import            Data.Typeable

import            Database.Orville.Core

data SignType = Inserted | Updated
  deriving (Eq, Show, Enum)

data Sign = forall entity. (Typeable entity) =>
  Sign {
    signType :: SignType
  , signTable :: TableDefinition entity
  , signEntity :: entity Record
  }

signTableAs :: Typeable entity
            => TableDefinition entity
            -> Sign
            -> Maybe (TableDefinition entity)
signTableAs _ (Sign _ tableDef _) = cast tableDef

signEntityFrom :: Typeable entity
               => TableDefinition entity
               -> Sign
               -> Maybe (entity Record)
signEntityFrom _ (Sign _ _ entity) = cast entity

instance Show Sign where
  show (Sign sType tableDef entity) =
    "Sign " <>
    show sType <> " " <>
    tableName tableDef <> " " <>
    show (tableGetKey tableDef entity)

newtype Trail = Trail (D.DList Sign)
  deriving (Monoid)

newtype TrackedOrville m a = TrackedOrville {
    unTrackedOrville :: WriterT Trail
                            m
                            a
  } deriving ( Functor
             , Alternative
             , Applicative
             , Monad
             , MonadPlus
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadError e
             )

runTrackedOrville :: TrackedOrville m a -> m (a, Trail)
runTrackedOrville = runWriterT . unTrackedOrville

trailToList :: Trail -> [Sign]
trailToList (Trail dList) = D.toList dList

class (MonadOrville conn m, MonadThrow m)
      => MonadTrackedOrville conn m where
  track :: Sign -> m ()

mapTrackedOrville :: (m (a,Trail) -> n (b,Trail)) -> TrackedOrville m a -> TrackedOrville n b
mapTrackedOrville f = TrackedOrville . mapWriterT f . unTrackedOrville

trackedMapper :: Monad m => (m a -> m b) -> m (a, Trail) -> m (b, Trail)
trackedMapper f trackedM = do
  (a, trail) <- trackedM
  b <- f (pure a)
  pure (b, trail)

untracked :: Monad m => m a -> TrackedOrville m a
untracked = TrackedOrville . lift

insertRecordTracked :: (MonadTrackedOrville conn m, Typeable entity)
                    => TableDefinition entity
                    -> entity ()
                    -> m (entity Record)
insertRecordTracked tableDef entity = do
  record <- insertRecord tableDef entity

  track $ Sign Inserted tableDef record

  pure record

updateRecordTracked :: (MonadTrackedOrville conn m, Typeable entity)
                    => TableDefinition entity
                    -> Record
                    -> entity key
                    -> m (entity Record)
updateRecordTracked tableDef recordId record = do
  updated <- updateRecord tableDef recordId record

  track $ Sign Updated tableDef updated

  pure updated

instance (MonadOrville conn m, MonadThrow m) => MonadTrackedOrville conn (TrackedOrville m) where
  track = TrackedOrville . tell . Trail . D.singleton

instance MonadTrans TrackedOrville where
  lift = untracked

instance MonadBase IO m => MonadBase IO (TrackedOrville m) where
  liftBase = lift . liftBase

instance MonadTransControl TrackedOrville where
  type StT TrackedOrville a = StT (WriterT Trail) a
  liftWith = defaultLiftWith TrackedOrville unTrackedOrville
  restoreT = defaultRestoreT TrackedOrville

instance MonadBaseControl IO m =>
         MonadBaseControl IO (TrackedOrville m) where
  type StM (TrackedOrville m) a = ComposeSt TrackedOrville m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadOrville conn m => MonadOrville conn (TrackedOrville m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv f  = mapTrackedOrville (localOrvilleEnv f)
  startTransactionSQL = lift startTransactionSQL

