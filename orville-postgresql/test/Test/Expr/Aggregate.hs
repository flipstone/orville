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
import qualified Test.Expr.TestSchema as TestSchema
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
    ]

prop_avgAggregate :: Property.NamedDBProperty
prop_avgAggregate =
  aggregateFunctionTest "avgAggregateFunction computes simple average" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 2.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.avgAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_maxAggregate :: Property.NamedDBProperty
prop_maxAggregate =
  aggregateFunctionTest "maxAggregateFunction computes maximum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 3.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.maxAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_minAggregate :: Property.NamedDBProperty
prop_minAggregate =
  aggregateFunctionTest "minAggregateFunction computes minimum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 1.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.minAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_sumAggregate :: Property.NamedDBProperty
prop_sumAggregate =
  aggregateFunctionTest "sumAggregateFunction computes simple sum" $
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 6.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.sumAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
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

stringAggByFooDescending :: Expr.ValueExpression
stringAggByFooDescending =
  Expr.stringAggAggregateFunction
    Nothing
    TestSchema.barColumnRef
    (Expr.valueExpression . SqlValue.fromText $ T.pack ", ")
    (Just fooDescending)
    Nothing

fooDescending :: Expr.OrderByClause
fooDescending =
  Expr.orderByClause $ Expr.orderByColumnName TestSchema.fooColumn Expr.descendingOrder

dogsAndDingo :: NE.NonEmpty TestSchema.FooBar
dogsAndDingo =
  NE.fromList [TestSchema.mkFooBar 1 "dog", TestSchema.mkFooBar 2 "dingo", TestSchema.mkFooBar 3 "dog"]

antCatBee :: NE.NonEmpty TestSchema.FooBar
antCatBee =
  NE.fromList [TestSchema.mkFooBar 1 "ant", TestSchema.mkFooBar 3 "cat", TestSchema.mkFooBar 2 "bee"]

data AggregateTest a = AggregateTest
  { aggregateTestDecodeResult :: SqlValue.SqlValue -> Either String a
  , aggregateTestExpectedResult :: a
  , aggregateTestValuesToInsert :: NE.NonEmpty TestSchema.FooBar
  , aggregateTestExpr :: Expr.ValueExpression
  }

aggregateFunctionTest ::
  (Show a, Eq a) =>
  String ->
  AggregateTest a ->
  Property.NamedDBProperty
aggregateFunctionTest testName aggregateTest =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- TestSchema.withFooBarData pool (aggregateTestValuesToInsert aggregateTest) $ \connection -> do
      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectDerivedColumns . pure $ Expr.deriveColumnAs (aggregateTestExpr aggregateTest) (RawSql.unsafeFromRawSql $ RawSql.fromString "agg"))
            (Just $ Expr.tableExpr (Expr.singleTableReferenceList TestSchema.fooBarTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    (fmap . fmap . fmap) (aggregateTestDecodeResult aggregateTest) rows
      HH.=== [[(Just (B8.pack "agg"), Right (aggregateTestExpectedResult aggregateTest))]]
