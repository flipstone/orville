module Main
  ( main,
    recheckDBProperty,
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString.Char8 as B8
import qualified Hedgehog as HH
import qualified System.Environment as Env
import qualified System.Exit as SE

import qualified Orville.PostgreSQL as Orville

import qualified Test.AutoMigration as AutoMigration
import qualified Test.Connection as Connection
import qualified Test.Cursor as Cursor
import qualified Test.EntityOperations as EntityOperations
import qualified Test.EntityTrace as EntityTrace
import qualified Test.Execution as Execution
import qualified Test.Expr.Count as ExprCount
import qualified Test.Expr.Cursor as ExprCursor
import qualified Test.Expr.InsertUpdateDelete as ExprInsertUpdateDelete
import qualified Test.Expr.Math as ExprMath
import qualified Test.Expr.OrderBy as ExprOrderBy
import qualified Test.Expr.SequenceDefinition as ExprSequenceDefinition
import qualified Test.Expr.TableDefinition as ExprTableDefinition
import qualified Test.Expr.Time as ExprTime
import qualified Test.Expr.Where as ExprWhere
import qualified Test.FieldDefinition as FieldDefinition
import qualified Test.MarshallError as MarshallError
import qualified Test.PgCatalog as PgCatalog
import qualified Test.PgTime as PgTime
import qualified Test.Plan as Plan
import qualified Test.PostgreSQLAxioms as PostgreSQLAxioms
import qualified Test.Property as Property
import qualified Test.RawSql as RawSql
import qualified Test.ReservedWords as ReservedWords
import qualified Test.SelectOptions as SelectOptions
import qualified Test.Sequence as Sequence
import qualified Test.SqlCommenter as SqlCommenter
import qualified Test.SqlMarshaller as SqlMarshaller
import qualified Test.SqlType as SqlType
import qualified Test.TableDefinition as TableDefinition
import qualified Test.Transaction as Transaction

main :: IO ()
main = do
  pool <- createTestConnectionPool

  summary <-
    Property.checkGroups
      [ Connection.connectionTests pool
      , RawSql.rawSqlTests
      , Execution.executionTests pool
      , SqlType.sqlTypeTests pool
      , PostgreSQLAxioms.postgreSQLAxiomTests pool
      , ExprInsertUpdateDelete.insertUpdateDeleteTests pool
      , ExprWhere.whereTests pool
      , ExprOrderBy.orderByTests pool
      , ExprTableDefinition.tableDefinitionTests pool
      , ExprSequenceDefinition.sequenceDefinitionTests pool
      , ExprCursor.cursorTests pool
      , ExprCount.countTests pool
      , ExprMath.mathTests pool
      , ExprTime.timeTests pool
      , FieldDefinition.fieldDefinitionTests pool
      , SqlMarshaller.sqlMarshallerTests
      , MarshallError.marshallErrorTests pool
      , TableDefinition.tableDefinitionTests pool
      , EntityOperations.entityOperationsTests pool
      , SelectOptions.selectOptionsTests
      , ReservedWords.reservedWordsTests pool
      , Transaction.transactionTests pool
      , Sequence.sequenceTests pool
      , Plan.planTests pool
      , PgCatalog.pgCatalogTests pool
      , AutoMigration.autoMigrationTests pool
      , EntityTrace.entityTraceTests pool
      , Cursor.cursorTests pool
      , SqlCommenter.sqlCommenterTests pool
      , PgTime.pgTimeTests pool
      ]

  Monad.unless (Property.allPassed summary) SE.exitFailure

createTestConnectionPool :: IO (Orville.Pool Orville.Connection)
createTestConnectionPool = do
  connStr <- lookupConnStr
  Orville.createConnectionPool Orville.DisableNoticeReporting 1 10 1 connStr

recheckDBProperty :: HH.Size -> HH.Seed -> Property.NamedDBProperty -> IO ()
recheckDBProperty size seed namedProperty = do
  pool <- createTestConnectionPool
  HH.recheck size seed (snd $ namedProperty pool)

lookupConnStr :: IO B8.ByteString
lookupConnStr = do
  mbConnHostStr <- Env.lookupEnv "TEST_CONN_HOST"
  let connStrUserPass = " user=orville_test password=orville"
  case mbConnHostStr of
    Nothing -> fail "TEST_CONN_HOST not set, so we don't know what database to connect to!"
    Just connHost -> pure . B8.pack $ connHost <> connStrUserPass
