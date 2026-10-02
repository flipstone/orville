module Test.Expr.Aggregate
  ( aggregateTests
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import Test.Expr.TestSchema (FooBar, barColumnRef, fooBarTable, fooColumn, fooColumnRef, mkFooBar, withFooBarData)
import qualified Test.Property as Property

aggregateTests :: Orville.ConnectionPool -> Property.Group
aggregateTests pool =
  Property.group
    "Expr - Aggregate"
    [ prop_avgAggregate pool
    , prop_maxAggregate pool
    , prop_minAggregate pool
    , prop_sumAggregate pool
    , prop_orderByFollowsParameterWithSpace
    , prop_stringAggOrderedWithParameterDelimiter pool
    , prop_arraySubscriptOfOrderedArrayAgg pool
    ]

prop_avgAggregate :: Property.NamedDBProperty
prop_avgAggregate =
  aggregateFunctionTest "avgAggregateFunction computes simple average" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 2.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.avgAggregateFunction Nothing fooColumnRef Nothing Nothing
      }

prop_maxAggregate :: Property.NamedDBProperty
prop_maxAggregate =
  aggregateFunctionTest "maxAggregateFunction computes maximum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 3.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.maxAggregateFunction Nothing fooColumnRef Nothing Nothing
      }

prop_minAggregate :: Property.NamedDBProperty
prop_minAggregate =
  aggregateFunctionTest "minAggregateFunction computes minimum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 1.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.minAggregateFunction Nothing fooColumnRef Nothing Nothing
      }

prop_sumAggregate :: Property.NamedDBProperty
prop_sumAggregate =
  aggregateFunctionTest "sumAggregateFunction computes simple sum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 6.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.sumAggregateFunction Nothing fooColumnRef Nothing Nothing
      }

prop_orderByFollowsParameterWithSpace :: Property.NamedProperty
prop_orderByFollowsParameterWithSpace =
  Property.singletonNamedProperty "aggregate ORDER BY is separated from a preceding parameter" $
    RawSql.toExampleBytes stringAggByFooDescending
      HH.=== B8.pack "\"string_agg\"(\"bar\",$1 ORDER BY \"foo\" DESC)"

prop_stringAggOrderedWithParameterDelimiter :: Property.NamedDBProperty
prop_stringAggOrderedWithParameterDelimiter =
  aggregateFunctionTest "stringAggAggregateFunction orders values when the delimiter is a parameter" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toText
      , aggregateTestExpectedResult = T.pack "cat, bee, ant"
      , aggregateTestValuesToInsert = antCatBee
      , aggregateTestExpr = stringAggByFooDescending
      }

prop_arraySubscriptOfOrderedArrayAgg :: Property.NamedDBProperty
prop_arraySubscriptOfOrderedArrayAgg =
  aggregateFunctionTest "arraySubscript selects an element of an ordered arrayAggAggregateFunction" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toText
      , aggregateTestExpectedResult = T.pack "bee"
      , aggregateTestValuesToInsert = antCatBee
      , aggregateTestExpr =
          Expr.arraySubscript
            (Expr.arrayAggAggregateFunction Nothing barColumnRef (Just fooDescending) Nothing)
            (Expr.valueExpression $ SqlValue.fromInt32 2)
      }

stringAggByFooDescending :: Expr.ValueExpression
stringAggByFooDescending =
  Expr.stringAggAggregateFunction
    Nothing
    barColumnRef
    (Expr.valueExpression . SqlValue.fromText $ T.pack ", ")
    (Just fooDescending)
    Nothing

fooDescending :: Expr.OrderByClause
fooDescending =
  Expr.orderByClause $ Expr.orderByColumnName fooColumn Expr.descendingOrder

dogsAndDingo :: NE.NonEmpty FooBar
dogsAndDingo =
  NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]

antCatBee :: NE.NonEmpty FooBar
antCatBee =
  NE.fromList [mkFooBar 1 "ant", mkFooBar 3 "cat", mkFooBar 2 "bee"]

data AggregateTest a = AggregateTest
  { aggregateTestDecodeResult :: SqlValue.SqlValue -> Either String a
  , aggregateTestExpectedResult :: a
  , aggregateTestValuesToInsert :: NE.NonEmpty FooBar
  , aggregateTestExpr :: Expr.ValueExpression
  }

aggregateFunctionTest ::
  (Show a, Eq a) =>
  String ->
  AggregateTest a ->
  Property.NamedDBProperty
aggregateFunctionTest testName aggregateTest =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- withFooBarData pool (aggregateTestValuesToInsert aggregateTest) $ \connection -> do
      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectDerivedColumns . pure $ Expr.deriveColumnAs (aggregateTestExpr aggregateTest) (RawSql.unsafeFromRawSql $ RawSql.fromString "agg"))
            (Just $ Expr.tableExpr (Expr.singleTableReferenceList fooBarTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    (fmap . fmap . fmap) (aggregateTestDecodeResult aggregateTest) rows
      HH.=== [[(Just (B8.pack "agg"), Right (aggregateTestExpectedResult aggregateTest))]]
