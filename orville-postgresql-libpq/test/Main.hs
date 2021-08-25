module Main
  ( main,
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as B8
import qualified System.Exit as SE

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Test.Connection as TestConnection
import qualified Test.EntityOperations as EntityOperations
import qualified Test.Expr.InsertUpdate as ExprInsertUpdate
import qualified Test.Expr.OrderBy as ExprOrderBy
import qualified Test.Expr.Where as ExprWhere
import qualified Test.FieldDefinition as FieldDefinition
import qualified Test.InformationSchema as InformationSchema
import qualified Test.RawSql as RawSql
import qualified Test.SelectOptions as SelectOptions
import qualified Test.SqlMarshaller as SqlMarshaller
import qualified Test.SqlType as SqlType
import qualified Test.TableDefinition as TableDefinition

main :: IO ()
main = do
  let connBStr = B8.pack "host=testdb user=orville_test password=orville"
  pool <- Connection.createConnectionPool 1 10 1 connBStr

  results <-
    sequence
      [ TestConnection.connectionTests pool
      , RawSql.rawSqlTests
      , TableDefinition.tableDefinitionTests pool
      , SqlMarshaller.sqlMarshallerTests
      , FieldDefinition.fieldDefinitionTests pool
      , EntityOperations.entityOperationsTests pool
      , ExprInsertUpdate.insertUpdateTests pool
      , ExprWhere.whereTests pool
      , ExprOrderBy.orderByTests pool
      , SqlType.sqlTypeTests pool
      , SelectOptions.selectOptionsTests
      , InformationSchema.informationSchemaTests pool
      ]
  Monad.unless (and results) SE.exitFailure
