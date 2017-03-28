module Database.Orville.Conduit
  ( selectConduit
  , selectSqlConduit
  ) where

import qualified  Control.Exception as E
import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.Trans
import            Data.Conduit
import qualified  Data.Conduit.List as C
import            Data.IORef
import qualified  Data.List as List
import            Data.Pool
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.SelectOptions
import            Database.Orville.Internal.TableDefinition
import            Database.Orville.Internal.Types

selectConduit :: (Monad m, MonadOrville conn m, MonadCatch m)
              => TableDefinition entity
              -> SelectOptions
              -> Source m (entity Record)
selectConduit tableDef opts =
    selectSqlConduit querySql (selectOptValues opts) =$=
      C.mapMaybe decodeRow
  where
    decodeRow row =
      case runFromSql (tableFromSql tableDef) row of
        Right ent -> Just ent
        Left _ -> Nothing

    selectClause = mkSelectClause tableDef
    querySql = List.intercalate " " [
                     selectClause
                   , selectOptClause opts
                   ]



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
selectSqlConduit :: (Monad m, MonadOrville conn m, MonadCatch m)
                 => String
                 -> [SqlValue]
                 -> Source m [(String, SqlValue)]
selectSqlConduit sql values = do
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
        query <- liftIO $ prepare conn sql
        addCleanup (const $ liftIO $ finish $ query) $ do
          liftIO $ putStrLn sql
          void $ liftIO $ execute query values
          feedRows query

  result <- go `onException` runCleanup
  runFinish
  pure $ result

feedRows :: (Monad m, MonadIO m)
         => Statement -> Source m [(String, SqlValue)]
feedRows query = do
  row <- liftIO $ fetchRowAL query
  case row of
     Nothing -> pure ()
     Just values -> do
       yield values
       feedRows query
