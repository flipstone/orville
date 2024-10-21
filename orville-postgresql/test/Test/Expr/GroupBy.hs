module Test.Expr.GroupBy
  ( groupByTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualSqlRows)
import qualified Test.Property as Property

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

groupByTests :: Orville.ConnectionPool -> Property.Group
groupByTests pool =
  Property.group
    "Expr - GroupBy"
    [ prop_groupByColumnsExpr pool
    , prop_appendGroupByExpr pool
    ]

prop_groupByColumnsExpr :: Property.NamedDBProperty
prop_groupByColumnsExpr =
  groupByTest "groupByColumnsExpr groups by columns" $
    GroupByTest
      { groupByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , groupByExpectedQueryResults = [FooBar 3 "dog", FooBar 2 "dingo", FooBar 1 "dog"]
      , groupByClause =
          Just . Expr.groupByClause $
            Expr.groupByColumnsExpr $
              barColumn :| [fooColumn]
      }

prop_appendGroupByExpr :: Property.NamedDBProperty
prop_appendGroupByExpr =
  groupByTest "appendGroupByExpr causes grouping on both clauses" $
    GroupByTest
      { groupByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dingo", FooBar 1 "dog", FooBar 2 "dingo"]
      , groupByExpectedQueryResults = [FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dingo"]
      , groupByClause =
          Just . Expr.groupByClause $
            Expr.appendGroupByExpr
              (Expr.groupByColumnsExpr . pure $ barColumn)
              (Expr.groupByColumnsExpr . pure $ fooColumn)
      }

data GroupByTest = GroupByTest
  { groupByValuesToInsert :: [FooBar]
  , groupByClause :: Maybe Expr.GroupByClause
  , groupByExpectedQueryResults :: [FooBar]
  }

mkGroupByTestInsertSource :: GroupByTest -> Expr.InsertSource
mkGroupByTestInsertSource test =
  let
    mkRow foobar =
      [ SqlValue.fromInt32 (foo foobar)
      , SqlValue.fromText (T.pack $ bar foobar)
      ]
  in
    Expr.insertSqlValues (fmap mkRow $ groupByValuesToInsert test)

mkGroupByTestExpectedRows :: GroupByTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkGroupByTestExpectedRows test =
  let
    mkRow foobar =
      [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
      , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
      ]
  in
    fmap mkRow (groupByExpectedQueryResults test)

groupByTest :: String -> GroupByTest -> Property.NamedDBProperty
groupByTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr testTable Nothing (mkGroupByTestInsertSource test) Nothing Nothing

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectColumns [fooColumn, barColumn])
            (Just $ Expr.tableExpr (Expr.tableFromItem testTable) Nothing (groupByClause test) Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    rows `assertEqualSqlRows` mkGroupByTestExpectedRows test

testTable :: Expr.Qualified Expr.TableName
testTable =
  Expr.qualifyTable Nothing (Expr.tableName "expr_test")

fooColumn :: Expr.Qualified Expr.ColumnName
fooColumn =
  Expr.aliasQualifyColumn Nothing (Expr.columnName "foo")

barColumn :: Expr.Qualified Expr.ColumnName
barColumn =
  Expr.aliasQualifyColumn Nothing (Expr.columnName "bar")

dropAndRecreateTestTable :: Orville.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
