{-|
Module    : Database.Orville.Oracle.Raw
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Raw
  ( selectSql
  , selectSqlRows
  , decodeSqlRows
  , ResultSet
  , updateSql
  , withConnection
  , withTransaction
  ) where

import Control.Exception (finally)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Database.HDBC hiding (withTransaction)

import Database.Orville.Oracle.Internal.Execute
import Database.Orville.Oracle.Internal.FromSql
import Database.Orville.Oracle.Internal.Monad
import Database.Orville.Oracle.Internal.Types
import Database.Orville.Oracle.Select

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
        liftFinally finally doAction rollbackUncommitted
