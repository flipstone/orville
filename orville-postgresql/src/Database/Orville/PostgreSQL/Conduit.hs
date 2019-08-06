{-|
Module    : Database.Orville.PostgreSQL.Conduit
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.PostgreSQL.Conduit
  ( selectConduit
  ) where

{-
  !!! WARNING !!!

  Basically this entire file is forked using conditional compilation on the
  version of conduit that is being used. Only 'feedRows' is shared below, and
  even that needs a different type signature. If you're changing this file,
  you should probably take the time to run some earlier LTS versions to double
  check that conduit support works correctly with different library versions.
-}

#if MIN_VERSION_conduit(1,3,0)
import Conduit
  ( Acquire
  , ReleaseType(..)
  , allocateAcquire
  , mkAcquire
  , mkAcquireType
  )

import Control.Monad (void)
import Control.Monad.Catch
import Control.Monad.Trans
import Control.Monad.Trans.Resource (MonadResource, release)
import Data.Conduit
import Data.Pool
import Database.HDBC hiding (withTransaction)

import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.Select
import Database.Orville.PostgreSQL.Internal.Types

{-|
   'selectConduit' provides a way to stream the results of a 'Select' query
   from the database one by one using the conduit library. You can 'fuse' the
   conduit built by this function with your own conduit pipeline to handle rows
   individually in whatever fashion you need (e.g. turning them into rows of
   CSV). This is useful if you want to be able to process many rows one by one
   without loading them all into memory at once. You can aggregate the results
   however you require as part of the conduit processing and then use
   'runConduit' (or 'runConduitRes') from the conduit library to execute the
   processing pipeline. Alternatively, your web server ('wai', 'servant', etc)
   may provide support for converting a conduit into a streaming HTTP response.
  -}
selectConduit ::
     (Monad m, MonadOrville conn m, MonadCatch m, MonadResource m)
  => Select row
  -> ConduitT () row m ()
selectConduit select = do
  pool <- ormEnvPool <$> lift getOrvilleEnv
  (releaseKey, query) <-
    allocateAcquire (acquireStatement pool (selectSql select))
  void $ liftIO $ execute query $ selectValues select
  result <- feedRows (selectBuilder select) query
  -- Note this doesn't use finally to release this, but it will be released
  -- automatically at the end of runResourceT. finally cannot be used here
  -- because Conduit doesn't offer MonadMask. Alternatively we could use
  -- withAllocate here, but that would require an UNLiftIO instance
  release releaseKey
  pure result

acquireConnection :: Pool conn -> Acquire conn
acquireConnection pool =
  fst <$> mkAcquireType (takeResource pool) releaseConnection
  where
    releaseConnection (conn, local) releaseType =
      case releaseType of
        ReleaseEarly -> putResource local conn
        ReleaseNormal -> putResource local conn
        ReleaseException -> destroyResource pool local conn

acquireStatement ::
     IConnection conn => Pool conn -> String -> Acquire Statement
acquireStatement pool sql = do
  conn <- acquireConnection pool
  mkAcquire (prepare conn sql) finish
#else
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import Data.Conduit
import Data.IORef
import Data.Pool
import Database.HDBC hiding (withTransaction)

import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.Select
import Database.Orville.PostgreSQL.Internal.Types

-- All the masking manual cleanup in this function amounts to a
-- poor man's ResourceT that I *hope* is correct. The constraints
-- hat lead to this are:
--
--   * The immediate purpose of conduit queries to to provide streaming
--     responses in a Happstack App
--
--   * Happstack does not offer a side-effect controlled streaming
--     response solution at the moment. It relies in Lazy Bytestrings
--
--   * The conduit lazy consume specifically warns that you need to
--     ensure you consume the whole list before ResourceT returns,
--     which I cannot guarantee in Happstack (in fact, I believe it
--     specifically will *not* happen that way)
--
--   * Data.Pool.withResource depends on MonadBaseControl, which
--     Conduit does not offer
--
--   * Conduit also does not offer MonadMask, so I cannot use
--     mask/restore in the normal way
--
-- So, we instead we mask exceptions while registering cleanup and
-- finish actions in vars while masked and then ensure those vars
-- are read and executed at the appropriate times.
--
selectConduit ::
     (Monad m, MonadOrville conn m, MonadCatch m) => Select row -> Source m row
selectConduit select = do
  pool <- ormEnvPool <$> lift getOrvilleEnv
  cleanupRef <- liftIO $ newIORef (pure ())
  finishRef <- liftIO $ newIORef (pure ())
  let acquire =
        lift $
        liftIO $
        E.mask_ $ do
          (conn, local) <- takeResource pool
          writeIORef cleanupRef $ destroyResource pool local conn
          writeIORef finishRef $ putResource local conn
          pure conn
      runCleanup = liftIO $ join (readIORef cleanupRef)
      runFinish = liftIO $ join (readIORef finishRef)
      go = do
        conn <- acquire
        query <- liftIO $ prepare conn $ selectSql select
        addCleanup (const $ liftIO $ finish $ query) $ do
          void $ liftIO $ execute query $ selectValues select
          feedRows (selectBuilder select) query
  result <- go `onException` runCleanup
  runFinish
  pure $ result
#endif

feedRows ::
#if MIN_VERSION_conduit(1,3,0)
     (Monad m, MonadIO m) => FromSql row -> Statement -> ConduitT () row m ()
#else
     (Monad m, MonadIO m) => FromSql row -> Statement -> Source m row
#endif
feedRows builder query = do
  row <- liftIO $ fetchRowAL query
  case runFromSql builder <$> row of
    Nothing -> pure ()
    Just (Left _) -> pure ()
    Just (Right r) -> yield r >> feedRows builder query
