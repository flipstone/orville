{-|
Module    : Database.Orville.PostgreSQL.Raw
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Raw
  ( selectSql
  , selectSqlRows
  , decodeSqlRows
  , ResultSet
  , updateSql
  , withConnection
  , withTransaction
  , withTransactionOverEnv
  ) where

import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Pool
import Database.HDBC hiding (withTransaction)

import Database.Orville.PostgreSQL.Internal.Execute
import Database.Orville.PostgreSQL.Internal.FromSql
import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.Types
import Database.Orville.PostgreSQL.Select

selectSqlRows :: MonadOrville conn m => String -> [SqlValue] -> m ResultSet
selectSqlRows sql values = runSelect $ selectQueryRawRows sql values

selectSql ::
     MonadOrville conn m
  => String
  -> [SqlValue]
  -> FromSql result
  -> m [result]
selectSql sql values builder = runSelect $ selectQueryRaw builder sql values

updateSql :: MonadOrville conn m => String -> [SqlValue] -> m Integer
updateSql sql values =
  withConnection $ \conn -> do
    executingSql UpdateQuery sql $ do run conn sql values

startTransaction :: ConnectionEnv conn -> ConnectionEnv conn
startTransaction c = c {ormTransactionOpen = True}

withTransaction :: MonadOrville conn m => m a -> m a
withTransaction =
  withTransactionOverEnv getOrvilleEnv (localOrvilleEnv . const)

-- | Variant of 'withTransaction' that does not use 'MonadOrville'. This is
-- useful if your context has multiple 'OrvilleEnv's and you want to have
-- transactions over them interdependently.
withTransactionOverEnv
  :: (IConnection conn, MonadIO m, MonadOrvilleControl m)
  => m (OrvilleEnv conn) -- ^ Get the env from the context
  -> (OrvilleEnv conn -> m a -> m a) -- ^ Set the env in the context
  -> m a -> m a
withTransactionOverEnv getEnv setEnv action = do
  ormEnv <- getEnv

  let doTransactionWithConn connected =
        let newOrmEnv = setConnectionEnv (startTransaction connected) ormEnv
         in doTransaction
              (ormConnection connected)
              newOrmEnv
              (setEnv newOrmEnv action)

  case ormEnvConnectionEnv ormEnv of
    Just connected ->
      if ormTransactionOpen connected
         then action
         else doTransactionWithConn connected

    Nothing -> do
      let pool = ormEnvPool ormEnv
      liftWithConnection (withResource pool) $ \conn ->
        let connected = newConnectionEnv conn
         in doTransactionWithConn connected

doTransaction :: (IConnection conn, MonadOrvilleControl m, MonadIO m)
              => conn -> OrvilleEnv conn -> m a -> m a
doTransaction conn ormEnv action = do
  let txnCallback = ormEnvTransactionCallback ormEnv
      startTran = ormEnvStartTransactionSQL ormEnv

  committed <- liftIO $ newIORef False

  let doAction = do
        liftIO $ do
          executeRaw =<< prepare conn startTran
          txnCallback TransactionStart
        value <- action
        liftIO $ do
          commit conn
          writeIORef committed True
          txnCallback TransactionCommit
        return value

      rollbackUncommitted =
        liftIO $ do
          finished <- readIORef committed
          unless finished $ do
            rollback conn
            txnCallback TransactionRollback

  liftFinally finally doAction rollbackUncommitted
