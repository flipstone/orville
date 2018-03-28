{-|
Module    : Database.Orville.Internal.MigrateIndex
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Internal.MigrateIndex
  ( createIndex
  , dropIndex
  , getIndexes
  ) where

import Control.Monad
import Data.Convertible
import Data.List
import Database.HDBC

import Database.Orville.Internal.Execute
import Database.Orville.Internal.Monad
import Database.Orville.Internal.Types

createIndex :: MonadOrville conn m => conn -> IndexDefinition -> m ()
createIndex conn (IndexDefinition {..}) = do
  let ddl =
        intercalate
          " "
          [ "CREATE"
          , if indexUnique
              then "UNIQUE"
              else ""
          , "INDEX"
          , indexName
          , "ON"
          , "\"" ++ indexTable ++ "\""
          , indexBody
          ]
  executingSql DDLQuery ddl $ void $ run conn ddl []

dropIndex :: MonadOrville conn m => conn -> String -> m ()
dropIndex conn name = do
  let ddl = "DROP INDEX " ++ name
  executingSql DDLQuery ddl $ void $ run conn ddl []

getIndexes :: IConnection conn => conn -> IO [String]
getIndexes conn = do
  query <-
    prepare
      conn
      "SELECT indexname FROM pg_indexes WHERE schemaname = 'public';"
  void $ execute query []
  map (convert . head) <$> fetchAllRows' query
