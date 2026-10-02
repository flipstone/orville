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
  aggregateFunctionTest "avgAggregateFunction computes simple average" SqlValue.toDouble 2.0 dogsAndDingo $
    Expr.avgAggregateFunction Nothing fooColumnRef Nothing Nothing

prop_maxAggregate :: Property.NamedDBProperty
prop_maxAggregate =
  aggregateFunctionTest "maxAggregateFunction computes maximum" SqlValue.toDouble 3.0 dogsAndDingo $
    Expr.maxAggregateFunction Nothing fooColumnRef Nothing Nothing

prop_minAggregate :: Property.NamedDBProperty
prop_minAggregate =
  aggregateFunctionTest "minAggregateFunction computes minimum" SqlValue.toDouble 1.0 dogsAndDingo $
    Expr.minAggregateFunction Nothing fooColumnRef Nothing Nothing

prop_sumAggregate :: Property.NamedDBProperty
prop_sumAggregate =
  aggregateFunctionTest "sumAggregateFunction computes simple sum" SqlValue.toDouble 6.0 dogsAndDingo $
    Expr.sumAggregateFunction Nothing fooColumnRef Nothing Nothing

prop_orderByFollowsParameterWithSpace :: Property.NamedProperty
prop_orderByFollowsParameterWithSpace =
  Property.singletonNamedProperty "aggregate ORDER BY is separated from a preceding parameter" $
    RawSql.toExampleBytes stringAggByFooDescending
      HH.=== B8.pack "\"string_agg\"(\"bar\",$1 ORDER BY \"foo\" DESC)"

prop_stringAggOrderedWithParameterDelimiter :: Property.NamedDBProperty
prop_stringAggOrderedWithParameterDelimiter =
  aggregateFunctionTest
    "stringAggAggregateFunction orders values when the delimiter is a parameter"
    SqlValue.toText
    (T.pack "cat, bee, ant")
    antCatBee
    stringAggByFooDescending

prop_arraySubscriptOfOrderedArrayAgg :: Property.NamedDBProperty
prop_arraySubscriptOfOrderedArrayAgg =
  aggregateFunctionTest
    "arraySubscript selects an element of an ordered arrayAggAggregateFunction"
    SqlValue.toText
    (T.pack "bee")
    antCatBee
    $ Expr.arraySubscript
      (Expr.arrayAggAggregateFunction Nothing barColumnRef (Just fooDescending) Nothing)
      (Expr.valueExpression $ SqlValue.fromInt32 2)

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

aggregateFunctionTest ::
  (Show a, Eq a) =>
  String ->
  (SqlValue.SqlValue -> Either String a) ->
  a ->
  NE.NonEmpty FooBar ->
  Expr.ValueExpression ->
  Property.NamedDBProperty
aggregateFunctionTest testName decode expected fooBars aggregateExpr =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <- withFooBarData pool fooBars $ \connection -> do
      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectDerivedColumns . pure $ Expr.deriveColumnAs aggregateExpr (RawSql.unsafeFromRawSql $ RawSql.fromString "agg"))
            (Just $ Expr.tableExpr (Expr.singleTableReferenceList fooBarTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    (fmap . fmap . fmap) decode rows HH.=== [[(Just (B8.pack "agg"), Right expected)]]
