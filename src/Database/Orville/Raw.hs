{-|
Module    : Database.Orville.Raw
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Raw
  ( selectSql
  , selectSqlRows
  , decodeSqlRows
  , ResultSet
  , updateSql
  , withConnection
  , withTransaction
  ) where

import Control.Exception.Lifted (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Database.HDBC hiding (withTransaction)

import Database.Orville.Internal.Execute
import Database.Orville.Internal.FromSql
import Database.Orville.Internal.Monad
import Database.Orville.Internal.Types
import Database.Orville.Select

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
withTransaction action =
  withConnectionEnv $ \connected ->
    if ormTransactionOpen connected
      then action
      else localOrvilleEnv
             (setConnectionEnv $ startTransaction connected)
             doTransaction
  where
    doTransaction =
      withConnection $ \conn -> do
        txnCallback <- ormEnvTransactionCallback <$> getOrvilleEnv
        committed <- liftIO $ newIORef False
        startTran <- startTransactionSQL
        let doAction = do
              liftIO $ do
                (executeRaw =<< prepare conn startTran)
                txnCallback TransactionStart
              value <- action
              liftIO $ do
                commit conn
                writeIORef committed True
                txnCallback TransactionCommit
              return value
        let rollbackUncommitted =
              liftIO $ do
                finished <- readIORef committed
                when (not finished) $ do
                  rollback conn
                  txnCallback TransactionRollback
        doAction `finally` rollbackUncommitted
