module Test.Expr.GroupByOrderBy
  ( groupByOrderByTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualSqlRows)
import qualified Test.Property as Property

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

groupByOrderByTests :: Orville.Pool Orville.Connection -> Property.Group
groupByOrderByTests pool =
  Property.group
    "Expr - GroupBy and OrderBy"
    [ prop_groupByOrderByExpr pool
    ]

prop_groupByOrderByExpr :: Property.NamedDBProperty
prop_groupByOrderByExpr =
  groupByOrderByTest "GroupBy and OrderBy clauses can be used together" $
    GroupByOrderByTest
      { valuesToInsert = [FooBar 1 "shiba", FooBar 2 "dingo", FooBar 1 "dog", FooBar 2 "dingo", FooBar 1 "shiba"]
      , expectedQueryResults = [FooBar 2 "dingo", FooBar 1 "dog", FooBar 1 "shiba"]
      , groupByClause =
          Just . Expr.groupByClause $
            Expr.appendGroupByExpr
              (Expr.groupByExpr $ RawSql.toRawSql barColumn)
              (Expr.groupByExpr $ RawSql.toRawSql fooColumn)
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByExpr (RawSql.toRawSql barColumn) Expr.ascendingOrder
      }

data GroupByOrderByTest = GroupByOrderByTest
  { valuesToInsert :: [FooBar]
  , groupByClause :: Maybe Expr.GroupByClause
  , orderByClause :: Maybe Expr.OrderByClause
  , expectedQueryResults :: [FooBar]
  }

mkGroupByOrderByTestInsertSource :: GroupByOrderByTest -> Expr.InsertSource
mkGroupByOrderByTestInsertSource test =
  let
    mkRow foobar =
      [ SqlValue.fromInt32 (foo foobar)
      , SqlValue.fromText (T.pack $ bar foobar)
      ]
  in
    Expr.insertSqlValues (map mkRow $ valuesToInsert test)

mkGroupByOrderByTestExpectedRows :: GroupByOrderByTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkGroupByOrderByTestExpectedRows test =
  let
    mkRow foobar =
      [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
      , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
      ]
  in
    fmap mkRow (expectedQueryResults test)

groupByOrderByTest :: String -> GroupByOrderByTest -> Property.NamedDBProperty
groupByOrderByTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- MIO.liftIO . Pool.withResource pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr testTable Nothing (mkGroupByOrderByTestInsertSource test) Nothing

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectColumns [fooColumn, barColumn])
            (Just $ Expr.tableExpr (Expr.referencesTable testTable) Nothing (groupByClause test) (orderByClause test) Nothing Nothing)

      Execution.readRows result

    rows `assertEqualSqlRows` mkGroupByOrderByTestExpectedRows test

testTable :: Expr.Qualified Expr.TableName
testTable =
  Expr.qualifyTable Nothing (Expr.tableName "expr_test")

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.columnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.columnName "bar"

dropAndRecreateTestTable :: Orville.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
