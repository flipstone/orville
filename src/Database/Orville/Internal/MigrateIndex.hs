{-# LANGUAGE RecordWildCards #-}
module Database.Orville.Internal.MigrateIndex
  ( createIndex
  , dropIndex
  , getIndexes
  ) where

import            Control.Monad
import            Data.Convertible
import            Data.List
import            Database.HDBC

import            Database.Orville.Internal.Types

createIndex :: IConnection conn => conn -> IndexDefinition -> IO ()
createIndex conn (IndexDefinition {..}) = do
  let ddl = intercalate " " [ "CREATE"
                            , if indexUnique then "UNIQUE" else ""
                            , "INDEX"
                            , indexName
                            , "ON"
                            , "\"" ++ indexTable ++ "\""
                            , indexBody
                            ]

  putStrLn ddl
  void $ run conn ddl []

dropIndex :: IConnection conn => conn -> String -> IO ()
dropIndex conn name = do
  let ddl = "DROP INDEX " ++ name
  putStrLn ddl
  void $ run conn ddl []

getIndexes :: IConnection conn => conn -> IO [String]
getIndexes conn = do
  query <- prepare conn "SELECT indexname FROM pg_indexes WHERE schemaname = 'public';"
  void $ execute query []
  map (convert . head) <$> fetchAllRows' query

