module Test.TableDefinition
  ( tableDefinitionTree,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Pool (Pool, withResource)
import Hedgehog ((===))
import qualified Hedgehog as HH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL.Connection (Connection, SqlExecutionError (sqlExecutionErrorSqlState))
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.SqlMarshaller (marshallResultFromSql)
import Database.Orville.PostgreSQL.Internal.TableDefinition (TableDefinition (tableMarshaller), mkInsertExpr, mkQueryExpr)

import qualified Test.Entities.Foo as Foo
import qualified Test.TestTable as TestTable

tableDefinitionTree :: Pool Connection -> TestTree
tableDefinitionTree pool =
  testGroup
    "TableDefinition"
    [ testProperty "Creates a table than can round trip an entity through it" . HH.property $ do
        originalFoo <- HH.forAll Foo.generate

        let insertFoo =
              mkInsertExpr Foo.table (originalFoo :| [])

            selectFoos =
              mkQueryExpr Foo.table Nothing Nothing

        foosFromDB <-
          liftIO . withResource pool $ \connection -> do
            TestTable.dropAndRecreateTableDef connection Foo.table
            RawSql.executeVoid connection (Expr.insertExprToSql insertFoo)
            result <- RawSql.execute connection (Expr.queryExprToSql selectFoos)
            marshallResultFromSql (tableMarshaller Foo.table) result

        foosFromDB === Right [originalFoo]
    , testProperty "Creates a primary key that rejects duplicate records" . HH.withTests 1 . HH.property $ do
        originalFoo <- HH.forAll Foo.generate

        let insertFoo =
              mkInsertExpr Foo.table (originalFoo :| [])

        result <- liftIO . try . withResource pool $ \connection -> do
          TestTable.dropAndRecreateTableDef connection Foo.table
          RawSql.executeVoid connection (Expr.insertExprToSql insertFoo)
          RawSql.executeVoid connection (Expr.insertExprToSql insertFoo)

        case result of
          Right () -> do
            HH.footnote "Expected 'executeVoid' to return failure, but it did not"
            HH.failure
          Left err ->
            sqlExecutionErrorSqlState err === Just (B8.pack "23505")
    ]
