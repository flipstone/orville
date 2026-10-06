module Test.Expr.Aggregate
  ( aggregateTests
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Test.Expr.TestSchema as TestSchema
import qualified Test.Property as Property

aggregateTests :: Orville.ConnectionPool -> Tasty.TestTree
aggregateTests pool =
  Tasty.testGroup
    "Expr - Aggregate"
    [ TastyHH.testProperty "avgAggregateFunction computes simple average" (prop_avgAggregate pool)
    , TastyHH.testProperty "maxAggregateFunction computes maximum" (prop_maxAggregate pool)
    , TastyHH.testProperty "minAggregateFunction computes minimum" (prop_minAggregate pool)
    , TastyHH.testProperty "sumAggregateFunction computes simple sum" (prop_sumAggregate pool)
    , TastyHH.testProperty "aggregate ORDER BY is separated from a preceding parameter" prop_orderByFollowsParameterWithSpace
    , TastyHH.testProperty "stringAggAggregateFunction orders values when the delimiter is a parameter" (prop_stringAggOrderedWithParameterDelimiter pool)
    ]

prop_avgAggregate :: Orville.ConnectionPool -> HH.Property
prop_avgAggregate pool =
  aggregateFunctionTest
    pool
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 2.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.avgAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_maxAggregate :: Orville.ConnectionPool -> HH.Property
prop_maxAggregate pool =
  aggregateFunctionTest
    pool
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 3.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.maxAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_minAggregate :: Orville.ConnectionPool -> HH.Property
prop_minAggregate pool =
  aggregateFunctionTest
    pool
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 1.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.minAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_sumAggregate :: Orville.ConnectionPool -> HH.Property
prop_sumAggregate pool =
  aggregateFunctionTest
    pool
    AggregateTest
      { aggregateTestDecodeResult = SqlValue.toDouble
      , aggregateTestExpectedResult = 6.0
      , aggregateTestValuesToInsert = dogsAndDingo
      , aggregateTestExpr = Expr.sumAggregateFunction Nothing TestSchema.fooColumnRef Nothing Nothing
      }

prop_orderByFollowsParameterWithSpace :: HH.Property
prop_orderByFollowsParameterWithSpace =
  Property.singletonProperty $
    RawSql.toExampleBytes stringAggByFooDescending
      HH.=== B8.pack "\"string_agg\"(\"bar\",$1 ORDER BY \"foo\" DESC)"

prop_stringAggOrderedWithParameterDelimiter :: Orville.ConnectionPool -> HH.Property
prop_stringAggOrderedWithParameterDelimiter pool =
  aggregateFunctionTest
    pool
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
  Orville.ConnectionPool ->
  AggregateTest a ->
  HH.Property
aggregateFunctionTest pool aggregateTest =
  Property.singletonProperty $ do
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
