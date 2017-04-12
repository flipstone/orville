module Database.Orville.Conduit
  ( selectConduit
  ) where

import qualified  Control.Exception as E
import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.Trans
import            Data.Conduit
import            Data.IORef
import            Data.Pool
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Select
import            Database.Orville.Internal.Types

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
selectConduit :: (Monad m, MonadOrville conn m, MonadCatch m)
              => Select row
              -> Source m row
selectConduit select = do
  pool <- ormEnvPool <$> lift getOrvilleEnv
  cleanupRef <- liftIO $ newIORef (pure ())
  finishRef  <- liftIO $ newIORef (pure ())

  let acquire = lift $ liftIO $ E.mask_ $ do
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

feedRows :: (Monad m, MonadIO m)
         => FromSql row -> Statement -> Source m row
feedRows builder query = do
  row <- liftIO $ fetchRowAL query
  case runFromSql builder <$> row of
     Nothing -> pure ()
     Just (Left _) -> pure ()
     Just (Right row) -> yield row >> feedRows builder query

