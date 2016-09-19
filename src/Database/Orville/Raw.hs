{-# LANGUAGE RankNTypes #-}
module Database.Orville.Raw
  ( selectSql
  , selectSqlRows
  , decodeSqlRows
  , ResultSet
  , updateSql
  , withConnection, withTransaction
  ) where

import            Control.Exception.Lifted (finally, throw)
import            Control.Monad
import            Control.Monad.IO.Class
import            Data.Maybe
import            Data.IORef
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Types

type ResultSet = [[(String, SqlValue)]]

selectSqlRows :: String -> [SqlValue] -> Orville ResultSet
selectSqlRows sql values = do
  withConnection $ \conn -> liftIO $ do
    putStrLn sql

    query <- prepare conn sql
    void $ execute query values
    fetchAllRowsAL' query

decodeSqlRows :: FromSql result -> ResultSet -> Orville [result]
decodeSqlRows builder rows =
  fmap catMaybes $ forM rows $ \row -> do
    case runFromSql builder row of
      Right result -> pure $ Just result

      (Left (RowDataError msg)) -> do
        liftIO $ putStrLn $ concat
          [ "** Warning ** Error converting row from sql: "
          , show msg
          , ". First column was was: "
          , maybe "<no columns present>" show (listToMaybe row)
          ]

        pure Nothing

      Left err -> throw err

selectSql :: String
          -> [SqlValue]
          -> FromSql result
          -> Orville [result]
selectSql sql values builder =
  selectSqlRows sql values >>= decodeSqlRows builder

updateSql :: String
          -> [SqlValue]
          -> Orville Integer
updateSql sql values =
  withConnection $ \conn -> liftIO $ do
    putStrLn sql
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

