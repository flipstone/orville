{-|
Module    : Database.Orville.Oracle.Internal.Execute
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Internal.Execute where

import Control.Monad.IO.Class
import Database.HDBC hiding (withTransaction)

import Database.Orville.Oracle.Internal.Monad

executingSql :: MonadOrville conn m => QueryType -> String -> IO a -> m a
executingSql queryType sql action = do
  runningQuery <- ormEnvRunningQuery <$> getOrvilleEnv
  liftIO $ runningQuery queryType sql (catchSqlErr sql action)

catchSqlErr :: String -> IO a -> IO a
catchSqlErr sql action =
  catchSql
    action
    (\e ->
       let updatedErr =
             SqlError
               (seState e)
               (seNativeError e)
               (seErrorMsg e ++ " SQL: " ++ sql)
        in throwSqlError updatedErr)
