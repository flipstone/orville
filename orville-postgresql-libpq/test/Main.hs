module Main
  ( main,
  )
where

import qualified Data.ByteString.Char8 as B8
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

import Database.Orville.PostgreSQL.Connection (createConnectionPool)
import Test.Connection (connectionTree)
import Test.Expr (exprSpecs)
import Test.FieldDefinition (fieldDefinitionTree)
import Test.RawSql (rawSqlSpecs)
import Test.RecordOperations (recordOperationsTree)
import Test.SqlMarshaller (sqlMarshallerTree)
import Test.SqlType (sqlTypeSpecs)
import Test.TableDefinition (tableDefinitionTree)

main :: IO ()
main = do
  let connBStr = B8.pack "host=testdb user=orville_test password=orville"
  pool <- createConnectionPool 1 10 1 connBStr

  specTree <-
    testSpec "specs" $ do
      rawSqlSpecs
      exprSpecs pool
      sqlTypeSpecs pool

  defaultMain $
    testGroup
      "Tests"
      [ connectionTree pool
      , specTree
      , sqlMarshallerTree
      , fieldDefinitionTree pool
      , tableDefinitionTree pool
      , recordOperationsTree pool
      ]
