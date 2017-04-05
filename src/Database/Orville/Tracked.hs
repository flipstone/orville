{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Database.Orville.Tracked
  ( Sign(..)
  , SignType(..)

  , signTableAs
  , signEntityAs
  , signEntityFrom
  , signEntityGet

  , TrackedOrville
  , MonadTrackedOrville (..)

  , insertRecordTracked
  , updateRecordTracked
  , deleteRecordTracked

  , mapTrackedOrville
  , runTrackedOrville
  , trackedMapper
  ) where

import            Control.Applicative
import            Control.Monad.Base
import            Control.Monad.Catch
import            Control.Monad.Except
import            Control.Monad.Trans.Control
import            Control.Monad.RWS.Strict
import qualified  Data.DList as D
import            Data.Typeable

import            Database.Orville.Core

data SignType = Inserted | Updated | Deleted
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

signEntityAs :: Typeable entity
             => p (entity Record)
             -> Sign
             -> Maybe (entity Record)
signEntityAs _ (Sign _ _ entity) = cast entity

signEntityFrom :: Typeable entity
               => TableDefinition entity
               -> Sign
               -> Maybe (entity Record)
signEntityFrom _ (Sign _ _ entity) = cast entity

signEntityGet :: Typeable entity
              => (entity Record -> a)
              -> Sign
              -> Maybe a
signEntityGet f sign =
    f <$> signEntityAs proxy sign
  where
    proxy = Nothing
    _ = f <$> proxy

instance Show Sign where
  show (Sign sType tableDef entity) =
    "Sign " <>
    show sType <> " " <>
    tableName tableDef <> " " <>
    show (tableGetKey tableDef entity)

newtype TrackedOrville t m a = TrackedOrville {
    unTrackedOrville :: RWST (Sign -> t) t () m a
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
             , MonadWriter t
             )

runTrackedOrville :: (Monoid t, Monad m)
                  => (Sign -> t)
                  -> TrackedOrville t m a
                  -> m (a, t)
runTrackedOrville mapSign trackedM =
  evalRWST (unTrackedOrville trackedM) mapSign ()


{-# INLINE discardState #-}
discardState :: Functor m => m (a, (), t) -> m (a, t)
discardState = fmap $ \(a, _, t) -> (a, t)

{-# INLINE conjureState #-}
conjureState :: Functor m => m (a, t) -> m (a, (), t)
conjureState = fmap $ \(a, t) -> (a, (), t)

class (MonadOrville conn m, MonadThrow m)
      => MonadTrackedOrville conn m where
  track :: Sign -> m ()

mapTrackedOrville :: (Functor m, Functor n)
                  => (m (a,t) -> n (b,t))
                  -> TrackedOrville t m a
                  -> TrackedOrville t n b
mapTrackedOrville f =
    TrackedOrville . mapRWST (liftMap f) . unTrackedOrville
  where
    {-# INLINE liftMap #-}
    liftMap h = conjureState . h . discardState

{-# INLINE trackedMapper #-}
trackedMapper :: Monad m => (m a -> m b) -> m (a, t) -> m (b, t)
trackedMapper f trackedM = do
  (a, trail) <- trackedM
  b <- f (pure a)
  pure (b, trail)

untracked :: (Monoid t, Monad m) => m a -> TrackedOrville t m a
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

deleteRecordTracked :: (MonadTrackedOrville conn m, Typeable entity)
                    => TableDefinition entity
                    -> entity Record
                    -> m ()
deleteRecordTracked tableDef record = do
  deleteRecord tableDef record
  track $ Sign Deleted tableDef record

instance (Monoid t, MonadOrville conn m, MonadThrow m) =>
         MonadTrackedOrville conn (TrackedOrville t m) where
  track sign = TrackedOrville $ do
    mapSign <- ask
    tell $ mapSign sign

instance Monoid t => MonadTrans (TrackedOrville t) where
  lift = untracked

instance (Monoid t, MonadBase IO m) => MonadBase IO (TrackedOrville t m) where
  liftBase = lift . liftBase

instance Monoid t => MonadTransControl (TrackedOrville t) where
  type StT (TrackedOrville t) a = StT (RWST (Sign -> t) t ()) a
  liftWith = defaultLiftWith TrackedOrville unTrackedOrville
  restoreT = defaultRestoreT TrackedOrville

instance (Monoid t, MonadBaseControl IO m) =>
         MonadBaseControl IO (TrackedOrville t m) where
  type StM (TrackedOrville t m) a = ComposeSt (TrackedOrville t) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance (Monoid t, MonadOrville conn m) =>
         MonadOrville conn (TrackedOrville t m) where
  getOrvilleEnv = lift getOrvilleEnv
  localOrvilleEnv f  = mapTrackedOrville (localOrvilleEnv f)
  startTransactionSQL = lift startTransactionSQL

