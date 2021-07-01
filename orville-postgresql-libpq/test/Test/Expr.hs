module Test.Expr
  ( exprSpecs,
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import Data.Pool (Pool, withResource)
import qualified Data.Text as T

import Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

data FooBar = FooBar
  { foo :: Int32
  , bar :: String
  }

exprSpecs :: Pool Connection -> Spec
exprSpecs pool =
  describe "Expr Tests" $ do
    whereSpecs pool
    orderBySpecs pool
    groupBySpecs pool

whereSpecs :: Pool Connection -> Spec
whereSpecs pool =
  describe "WhereClause" $ do
    it "Returns all rows when where clause is specified" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereClause = Nothing
          }

    it "equalsOp matches exact value" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 2 "bee"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.columnEquals fooColumn (SqlValue.fromInt32 2)
          }

    it "greaterThanOp matches greater values" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 3 "chihuahua"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.columnGreaterThan fooColumn (SqlValue.fromInt32 2)
          }

    it "greaterThanOrEqualsOp matches greater or equal values" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.columnGreaterThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
          }

    it "lessThanOp matches lesser values" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 1 "ant"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.columnLessThan fooColumn (SqlValue.fromInt32 2)
          }

    it "lessThanOrEqualsOp matches lesser or equal values" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
          , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.columnLessThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
          }

    it "andExpr requires both conditions to be true" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
          , whereExpectedQueryResults = [FooBar 3 "dog"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.andExpr
                  (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                  (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))
          }

    it "orExpr requires either conditions to be true" $ do
      runWhereConditionTest pool $
        WhereConditionTest
          { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
          , whereExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog"]
          , whereClause =
              Just . Expr.whereClause $
                Expr.orExpr
                  (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                  (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dingo")))
          }

orderBySpecs :: Pool Connection -> Spec
orderBySpecs pool =
  describe "OrderBy" $ do
    it "ascendingExpr sorts a text column" $ do
      runOrderByTest pool $
        OrderByTest
          { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
          , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dog"]
          , orderByClause =
              Just . Expr.orderByClause $
                Expr.orderByExpr
                  (Expr.columnNameToSql barColumn)
                  Expr.ascendingOrder
          }

    it "descendingExpr sorts a text column" $ do
      runOrderByTest pool $
        OrderByTest
          { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
          , orderByExpectedQueryResults = [FooBar 1 "dog", FooBar 3 "dog", FooBar 2 "dingo"]
          , orderByClause =
              Just . Expr.orderByClause $
                Expr.orderByExpr
                  (Expr.columnNameToSql barColumn)
                  Expr.descendingOrder
          }

    it "addOrderBy causes ordering on both columns" $ do
      runOrderByTest pool $
        OrderByTest
          { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
          , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog", FooBar 1 "dog"]
          , orderByClause =
              Just . Expr.orderByClause $
                Expr.appendOrderBy
                  (Expr.orderByExpr (Expr.columnNameToSql barColumn) Expr.ascendingOrder)
                  (Expr.orderByExpr (Expr.columnNameToSql fooColumn) Expr.descendingOrder)
          }

data OrderByTest = OrderByTest
  { orderByValuesToInsert :: [FooBar]
  , orderByClause :: Maybe Expr.OrderByClause
  , orderByExpectedQueryResults :: [FooBar]
  }

mkOrderByTestInsertSource :: OrderByTest -> Expr.InsertSource
mkOrderByTestInsertSource test =
  let mkRow foobar =
        [ SqlValue.fromInt32 (foo foobar)
        , SqlValue.fromText (T.pack $ bar foobar)
        ]
   in Expr.insertSqlValues (map mkRow $ orderByValuesToInsert test)

mkOrderByTestExpectedRows :: OrderByTest -> [[(Maybe B8.ByteString, SqlValue)]]
mkOrderByTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (orderByExpectedQueryResults test)

runOrderByTest :: Pool Connection -> OrderByTest -> IO ()
runOrderByTest pool test =
  withResource pool $ \connection -> do
    dropAndRecreateTestTable connection

    let exprTestTable = Expr.rawTableName "expr_test"

    RawSql.executeVoid connection
      . Expr.insertExprToSql
      $ Expr.insertExpr exprTestTable Nothing (mkOrderByTestInsertSource test)

    result <-
      RawSql.execute connection
        . Expr.queryExprToSql
        $ Expr.queryExpr
          (Expr.selectColumns [fooColumn, barColumn])
          (Expr.tableExpr exprTestTable Nothing (orderByClause test) Nothing)

    rows <- ExecResult.readRows result
    rows `shouldBe` mkOrderByTestExpectedRows test

groupBySpecs :: Pool Connection -> Spec
groupBySpecs pool =
  describe "GroupBy" $ do
    it "appendGroupBy causes grouping on both clauses" $ do
      runGroupByTest pool $
        GroupByTest
          { groupByValuesToInsert = [FooBar 1 "dog", FooBar 2 "cat", FooBar 1 "dog", FooBar 3 "cat", FooBar 1 "dog", FooBar 2 "cat"]
          , groupByExpectedQueryResults = [FooBar 1 "dog", FooBar 3 "cat", FooBar 2 "cat"]
          , groupByClause =
              Just . Expr.groupByClause $
                Expr.appendGroupBy
                  (Expr.groupByExpr (Expr.columnNameToSql barColumn))
                  (Expr.groupByExpr (Expr.columnNameToSql fooColumn))
          }

data GroupByTest = GroupByTest
  { groupByValuesToInsert :: [FooBar]
  , groupByClause :: Maybe Expr.GroupByClause
  , groupByExpectedQueryResults :: [FooBar]
  }

mkGroupByTestInsertSource :: GroupByTest -> Expr.InsertSource
mkGroupByTestInsertSource test =
  let mkRow foobar =
        [ SqlValue.fromInt32 (foo foobar)
        , SqlValue.fromText (T.pack $ bar foobar)
        ]
   in Expr.insertSqlValues (map mkRow $ groupByValuesToInsert test)

mkGroupByTestExpectedRows :: GroupByTest -> [[(Maybe B8.ByteString, SqlValue)]]
mkGroupByTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (groupByExpectedQueryResults test)

runGroupByTest :: Pool Connection -> GroupByTest -> IO ()
runGroupByTest pool test =
  withResource pool $ \connection -> do
    dropAndRecreateTestTable connection

    let exprTestTable = Expr.rawTableName "expr_test"

    RawSql.executeVoid connection
      . Expr.insertExprToSql
      $ Expr.insertExpr exprTestTable Nothing (mkGroupByTestInsertSource test)

    result <-
      RawSql.execute connection
        . Expr.queryExprToSql
        $ Expr.queryExpr
          (Expr.selectColumns [fooColumn, barColumn])
          (Expr.tableExpr exprTestTable Nothing Nothing (groupByClause test))

    rows <- ExecResult.readRows result
    rows `shouldBe` mkGroupByTestExpectedRows test

data WhereConditionTest = WhereConditionTest
  { whereValuesToInsert :: [FooBar]
  , whereClause :: Maybe Expr.WhereClause
  , whereExpectedQueryResults :: [FooBar]
  }

mkTestInsertSource :: WhereConditionTest -> Expr.InsertSource
mkTestInsertSource test =
  let mkRow foobar =
        [ SqlValue.fromInt32 (foo foobar)
        , SqlValue.fromText (T.pack $ bar foobar)
        ]
   in Expr.insertSqlValues (map mkRow $ whereValuesToInsert test)

mkTestExpectedRows :: WhereConditionTest -> [[(Maybe B8.ByteString, SqlValue)]]
mkTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (whereExpectedQueryResults test)

runWhereConditionTest :: Pool Connection -> WhereConditionTest -> IO ()
runWhereConditionTest pool test =
  withResource pool $ \connection -> do
    dropAndRecreateTestTable connection

    let exprTestTable = Expr.rawTableName "expr_test"

    RawSql.executeVoid connection $
      Expr.insertExprToSql $
        Expr.insertExpr exprTestTable Nothing (mkTestInsertSource test)

    result <-
      RawSql.execute connection $
        Expr.queryExprToSql $
          Expr.queryExpr
            (Expr.selectColumns [fooColumn, barColumn])
            (Expr.tableExpr exprTestTable (whereClause test) Nothing Nothing)

    rows <- ExecResult.readRows result
    rows `shouldBe` mkTestExpectedRows test

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "expr_test"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.rawColumnName "bar"

dropAndRecreateTestTable :: Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> Expr.tableNameToSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
