{-# LANGUAGE RankNTypes #-}
module Database.Orville.Raw
  ( selectSql
  , selectSqlRows
  , decodeSqlRows
  , ResultSet
  , updateSql
  , withConnection, withTransaction
  ) where

import            Control.Exception.Lifted (finally)
import            Control.Monad
import            Control.Monad.IO.Class
import            Data.IORef
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Execute
import            Database.Orville.Internal.FromSql
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Types
import            Database.Orville.Select

selectSqlRows :: String -> [SqlValue] -> Orville ResultSet
selectSqlRows sql values =
  runSelect $ selectQueryRawRows sql values

selectSql :: String
          -> [SqlValue]
          -> FromSql result
          -> Orville [result]
selectSql sql values builder =
  runSelect $ selectQueryRaw builder sql values

updateSql :: String
          -> [SqlValue]
          -> Orville Integer
updateSql sql values =
  withConnection $ \conn -> do
    executingSql UpdateQuery sql $ do
      run conn sql values

startTransaction :: ConnectionEnv conn -> ConnectionEnv conn
startTransaction c = c { ormTransactionOpen = True }

withTransaction :: MonadOrville conn m => m a -> m a
withTransaction action =
    withConnectionEnv $ \connected ->
      if ormTransactionOpen connected then
        action
      else
        localOrvilleEnv (setConnectionEnv $ startTransaction connected)
                      doTransaction

  where
    doTransaction =
      withConnection $ \conn -> do
        committed <- liftIO $ newIORef False
        startTran <- startTransactionSQL

        let doAction = do liftIO $ (executeRaw =<< prepare conn startTran)
                          value <- action
                          liftIO $ commit conn >> (writeIORef committed True)
                          return value

        let rollbackUncommitted = liftIO $ do
                                    finished <- readIORef committed
                                    when (not finished) (rollback conn)

        doAction `finally` rollbackUncommitted

