module Test.Expr.Aggregate
  ( aggregateTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

newtype DoubleRes = DoubleRes
  {doubleRes :: Double}

aggregateTests :: Orville.ConnectionPool -> Property.Group
aggregateTests pool =
  Property.group
    "Expr - Aggregate"
    [ prop_avgAggregate pool
    , prop_maxAggregate pool
    , prop_minAggregate pool
    , prop_sumAggregate pool
    ]

prop_avgAggregate :: Property.NamedDBProperty
prop_avgAggregate =
  aggregateFunctionTest "avgAggregateFunction computes simple average" $
    AggregateTest
      { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , groupByExpectedQueryResults =
          [ DoubleRes 2.00000
          ]
      , aggregateFunctionExpr =
          Expr.avgAggregateFunction
            Nothing
            (Expr.columnReference fooColumn)
            Nothing
            Nothing
      }

prop_maxAggregate :: Property.NamedDBProperty
prop_maxAggregate =
  aggregateFunctionTest "maxAggregateFunction computes maximum" $
    AggregateTest
      { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , groupByExpectedQueryResults =
          [ DoubleRes 3.0
          ]
      , aggregateFunctionExpr =
          Expr.maxAggregateFunction
            Nothing
            (Expr.columnReference fooColumn)
            Nothing
            Nothing
      }

prop_minAggregate :: Property.NamedDBProperty
prop_minAggregate =
  aggregateFunctionTest "minAggregateFunction computes minimum" $
    AggregateTest
      { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , groupByExpectedQueryResults =
          [ DoubleRes 1.0
          ]
      , aggregateFunctionExpr =
          Expr.minAggregateFunction
            Nothing
            (Expr.columnReference fooColumn)
            Nothing
            Nothing
      }

prop_sumAggregate :: Property.NamedDBProperty
prop_sumAggregate =
  aggregateFunctionTest "sumAggregateFunction computes simple sum" $
    AggregateTest
      { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , groupByExpectedQueryResults =
          [ DoubleRes 6.0
          ]
      , aggregateFunctionExpr =
          Expr.sumAggregateFunction
            Nothing
            (Expr.columnReference fooColumn)
            Nothing
            Nothing
      }

data AggregateTest = AggregateTest
  { valuesToInsert :: [FooBar]
  , aggregateFunctionExpr :: Expr.ValueExpression
  , groupByExpectedQueryResults :: [DoubleRes]
  }

aggregateFunctionTest :: String -> AggregateTest -> Property.NamedDBProperty
aggregateFunctionTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      dropAndRecreateTestTable connection

      RawSql.executeVoid connection $
        Expr.insertExpr testTable Nothing (mkAggregateTestInsertSource test) Nothing Nothing

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectDerivedColumns . pure $ Expr.deriveColumnAsAlias (aggregateFunctionExpr test) (RawSql.unsafeFromRawSql $ RawSql.fromString "agg"))
            (Just $ Expr.tableExpr (Expr.tableFromItem testTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    ((fmap . fmap . fmap) SqlValue.toDouble rows) HH.=== ((fmap . fmap . fmap) SqlValue.toDouble $ mkAggregateTestExpectedRows test)

mkAggregateTestInsertSource :: AggregateTest -> Expr.InsertSource
mkAggregateTestInsertSource test =
  let
    mkRow foobar =
      [ SqlValue.fromInt32 (foo foobar)
      , SqlValue.fromText (T.pack $ bar foobar)
      ]
  in
    Expr.insertSqlValues (fmap mkRow $ valuesToInsert test)

mkAggregateTestExpectedRows :: AggregateTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkAggregateTestExpectedRows test =
  let
    mkRow res =
      [ (Just (B8.pack "agg"), SqlValue.fromDouble (doubleRes res))
      ]
  in
    fmap mkRow (groupByExpectedQueryResults test)

testTable :: Expr.QualifiedOrUnqualified Expr.TableName
testTable =
  Expr.unqualified (Expr.tableName "expr_test")

fooColumn :: Expr.QualifiedOrUnqualified Expr.ColumnName
fooColumn =
  Expr.unqualified (Expr.columnName "foo")

dropAndRecreateTestTable :: Orville.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> RawSql.toRawSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
