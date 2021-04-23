module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as B8
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (testSpec)

import Database.Orville.PostgreSQL.Connection (createConnectionPool)
import Test.RawSql (rawSqlSpecs)
import Test.SqlType (sqlTypeSpecs)

main :: IO ()
main = do
  let connBStr = B8.pack "host=testdb user=orville_test password=orville"
  pool <- createConnectionPool 1 10 1 connBStr

  testTree <-
    testSpec "specs" $ do
      sqlTypeSpecs pool
      rawSqlSpecs

  defaultMain testTree
