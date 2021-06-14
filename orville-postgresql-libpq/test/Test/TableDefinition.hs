module Test.TableDefinition
  ( tableDefinitionTree,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Pool (Pool)
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL.Connection (Connection, SqlExecutionError (sqlExecutionErrorSqlState))
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, NotNull, integerField, unboundedTextField)
import Database.Orville.PostgreSQL.Internal.PrimaryKey (primaryKey)
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.SqlMarshaller (SqlMarshaller, marshallField, marshallResultFromSql)
import Database.Orville.PostgreSQL.Internal.TableDefinition (TableDefinition (..), mkCreateTableExpr, mkInsertExpr, mkQueryExpr)

import qualified Test.PGGen as PGGen

tableDefinitionTree :: Pool Connection -> TestTree
tableDefinitionTree pool =
  testGroup
    "TableDefinition"
    [ testProperty "Creates a table than can round trip an entity through it" . HH.property $ do
        originalFoo <- HH.forAll generateFoo

        let insertFoo =
              mkInsertExpr fooTable (originalFoo :| [])

            selectFoos =
              mkQueryExpr fooTable Nothing Nothing

        foosFromDB <-
          liftIO $ do
            dropAndRecreateTestTable pool fooTable
            RawSql.executeVoid pool (Expr.insertExprToSql insertFoo)
            result <- RawSql.execute pool (Expr.queryExprToSql selectFoos)
            marshallResultFromSql (tableMarshaller fooTable) result

        foosFromDB === Right [originalFoo]
    , testProperty "Creates a primary key that rejects duplicate records" . HH.withTests 1 . HH.property $ do
        originalFoo <- HH.forAll generateFoo

        let insertFoo =
              mkInsertExpr fooTable (originalFoo :| [])

        result <- liftIO . try $ do
          dropAndRecreateTestTable pool fooTable
          RawSql.executeVoid pool (Expr.insertExprToSql insertFoo)
          RawSql.executeVoid pool (Expr.insertExprToSql insertFoo)

        case result of
          Right () -> do
            HH.footnote "Expected 'executeVoid' to return failure, but it did not"
            HH.failure
          Left err ->
            sqlExecutionErrorSqlState err === Just (B8.pack "23505")
    ]

fooTable :: TableDefinition FooId Foo Foo
fooTable =
  TableDefinition
    { tableName = Expr.rawTableName "foo"
    , tablePrimaryKey = primaryKey fooIdField
    , tableMarshaller = fooMarshaller
    }

fooMarshaller :: SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> marshallField fooId fooIdField
    <*> marshallField fooName fooNameField

fooIdField :: FieldDefinition NotNull FooId
fooIdField =
  integerField "id"

fooNameField :: FieldDefinition NotNull FooName
fooNameField =
  unboundedTextField "name"

type FooId = Int32
type FooName = T.Text

data Foo = Foo
  { fooId :: FooId
  , fooName :: FooName
  }
  deriving (Eq, Show)

generateFoo :: HH.Gen Foo
generateFoo =
  Foo
    <$> PGGen.pgInt32
    <*> PGGen.pgText (Range.constant 0 10)

dropAndRecreateTestTable ::
  Pool Connection ->
  TableDefinition key writeEntity readEntity ->
  IO ()
dropAndRecreateTestTable pool tableDef = do
  RawSql.executeVoid pool (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql (tableName tableDef))
  RawSql.executeVoid pool (Expr.createTableExprToSql $ mkCreateTableExpr tableDef)
