{-|
Module    : Database.Orville.Tracked
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
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
  , MonadTrackedOrville(..)
  , insertRecordTracked
  --, updateRecordTracked
  , deleteRecordTracked
  , mapTrackedOrville
  , runTrackedOrville
  , trackedMapper
  ) where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Control
import Data.Typeable

import Database.Orville.Core

data SignType
  = Inserted
  | Updated
  | Deleted
  deriving (Eq, Show, Enum)

data Sign = forall key readEntity writeEntity. ( Typeable readEntity
                                               , Typeable writeEntity
                                               , Typeable key
                                               ) =>
                                               Sign
  { signType :: SignType
  , signTable :: TableDefinition readEntity writeEntity key
  , signEntity :: readEntity
  }

signTableAs ::
     (Typeable readEntity, Typeable writeEntity, Typeable key)
  => TableDefinition readEntity writeEntity key
  -> Sign
  -> Maybe (TableDefinition readEntity writeEntity key)
signTableAs _ (Sign _ tableDef _) = cast tableDef

signEntityAs ::
     (Typeable readEntity) => p readEntity -> Sign -> Maybe readEntity
signEntityAs _ (Sign _ _ entity) = cast entity

signEntityFrom ::
     (Typeable readEntity, Typeable writeEntity, Typeable key)
  => TableDefinition readEntity writeEntity key
  -> Sign
  -> Maybe readEntity
signEntityFrom _ (Sign _ _ entity) = cast entity

signEntityGet :: Typeable readEntity => (readEntity -> a) -> Sign -> Maybe a
signEntityGet f sign = f <$> signEntityAs proxy sign
  where
    proxy = Nothing
    _ = f <$> proxy

instance Show Sign where
  show (Sign sType tableDef entity) =
    "Sign " <> show sType <> " " <> tableName tableDef <> " " <>
    show (tableKeyToSql tableDef (tableGetKey tableDef entity))

newtype TrackedOrville t m a = TrackedOrville
  { unTrackedOrville :: RWST (Sign -> t) t () m a
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

runTrackedOrville ::
     (Monoid t, Monad m) => (Sign -> t) -> TrackedOrville t m a -> m (a, t)
runTrackedOrville mapSign trackedM =
  evalRWST (unTrackedOrville trackedM) mapSign ()

{-# INLINE discardState #-}
discardState :: Functor m => m (a, (), t) -> m (a, t)
discardState = fmap $ \(a, _, t) -> (a, t)

{-# INLINE conjureState #-}
conjureState :: Functor m => m (a, t) -> m (a, (), t)
conjureState = fmap $ \(a, t) -> (a, (), t)

class (MonadOrville conn m, MonadThrow m) =>
      MonadTrackedOrville conn m
  where
  track :: Sign -> m ()

mapTrackedOrville ::
     (Functor m, Functor n)
  => (m (a, t) -> n (b, t))
  -> TrackedOrville t m a
  -> TrackedOrville t n b
mapTrackedOrville f = TrackedOrville . mapRWST (liftMap f) . unTrackedOrville
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

insertRecordTracked ::
     ( MonadTrackedOrville conn m
     , Typeable readEntity
     , Typeable writeEntity
     , Typeable key
     )
  => TableDefinition readEntity writeEntity key
  -> writeEntity
  -> m readEntity
insertRecordTracked tableDef entity = do
  record <- insertRecord tableDef entity
  track $ Sign Inserted tableDef record
  pure record

--
-- I have commented this out for this spike because it needs the readEntity that
-- used to be returned by updateRecord but is no longer returned because of the
-- switch to updating via partial entity. This would need to be solved in some way
-- for this approach to work. One approach would be to only allow tracking on
-- the fields in the partial entity, but that seems possibly too restrictive.
--
--updateRecordTracked ::
--     ( MonadTrackedOrville conn m
--     , Typeable readEntity
--     , Typeable writeEntity
--     , Typeable key
--     )
--  => TableDefinition readEntity writeEntity key
--  -> key
--  -> writeEntity
--  -> m ()
--updateRecordTracked tableDef key record = do
--  updateRecord tableDef key record
--  track $ Sign Updated tableDef updated
deleteRecordTracked ::
     ( MonadTrackedOrville conn m
     , Typeable readEntity
     , Typeable writeEntity
     , Typeable key
     )
  => TableDefinition readEntity writeEntity key
  -> readEntity
  -> m ()
deleteRecordTracked tableDef record = do
  deleteRecord tableDef record
  track $ Sign Deleted tableDef record

instance (Monoid t, MonadOrville conn m, MonadThrow m) =>
         MonadTrackedOrville conn (TrackedOrville t m) where
  track sign =
    TrackedOrville $ do
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
  localOrvilleEnv f = mapTrackedOrville (localOrvilleEnv f)
  startTransactionSQL = lift startTransactionSQL
