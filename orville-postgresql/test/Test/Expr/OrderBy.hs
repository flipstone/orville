module Test.Expr.OrderBy
  ( orderByTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.List.NonEmpty as NE
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, dropAndRecreateTestTable, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

orderByTests :: Orville.ConnectionPool -> Tasty.TestTree
orderByTests pool =
  Tasty.testGroup
    "Expr - OrderBy"
    [ TastyHH.testProperty "ascendingExpr sorts a text column" (prop_ascendingExpr pool)
    , TastyHH.testProperty "descendingExpr sorts a text column" (prop_descendingExpr pool)
    , TastyHH.testProperty "appendOrderByExpr causes ordering on both columns" (prop_appendOrderByExpr pool)
    , TastyHH.testProperty "orderByColumnsExpr orders by columns" (prop_orderByColumnsExpr pool)
    , TastyHH.testProperty "ascendingOrderWith sorts columns with nulls first/last" (prop_ascendingOrderWithExpr pool)
    , TastyHH.testProperty "descendingOrderWith sorts columns with nulls first/last" (prop_descendingOrderWithExpr pool)
    , TastyHH.testProperty "descendingExpr sorts a text column as expected with distinctOn" (prop_distinctOnExpr pool)
    ]

prop_ascendingExpr :: Orville.ConnectionPool -> HH.Property
prop_ascendingExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 1 "dog", mkFooBar 3 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnName barColumn Expr.ascendingOrder
      }

prop_descendingExpr :: Orville.ConnectionPool -> HH.Property
prop_descendingExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 3 "dog", mkFooBar 2 "dingo"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnName barColumn Expr.descendingOrder
      }

prop_appendOrderByExpr :: Orville.ConnectionPool -> HH.Property
prop_appendOrderByExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog", mkFooBar 1 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.appendOrderByExpr
              (Expr.orderByColumnName barColumn Expr.ascendingOrder)
              (Expr.orderByColumnName fooColumn Expr.descendingOrder)
      }

prop_orderByColumnsExpr :: Orville.ConnectionPool -> HH.Property
prop_orderByColumnsExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog", mkFooBar 1 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnsExpr $
              (barColumn, Expr.ascendingOrder)
                NE.:| [(fooColumn, Expr.descendingOrder)]
      }

prop_ascendingOrderWithExpr :: Orville.ConnectionPool -> HH.Property
prop_ascendingOrderWithExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert =
          NE.fromList [FooBar Nothing Nothing, FooBar (Just 1) Nothing, mkFooBar 2 "dog", FooBar Nothing (Just "dog")]
      , orderByExpectedQueryResults =
          [FooBar Nothing (Just "dog"), FooBar Nothing Nothing, FooBar (Just 1) Nothing, mkFooBar 2 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.appendOrderByExpr
              (Expr.orderByColumnName fooColumn $ Expr.ascendingOrderWith Expr.NullsFirst)
              (Expr.orderByColumnName barColumn $ Expr.ascendingOrderWith Expr.NullsLast)
      }

prop_descendingOrderWithExpr :: Orville.ConnectionPool -> HH.Property
prop_descendingOrderWithExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert =
          NE.fromList [FooBar Nothing Nothing, FooBar (Just 1) Nothing, mkFooBar 2 "dog", FooBar Nothing (Just "dog")]
      , orderByExpectedQueryResults =
          [FooBar Nothing (Just "dog"), FooBar Nothing Nothing, mkFooBar 2 "dog", FooBar (Just 1) Nothing]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.appendOrderByExpr
              (Expr.orderByColumnName fooColumn $ Expr.descendingOrderWith Expr.NullsFirst)
              (Expr.orderByColumnName barColumn $ Expr.descendingOrderWith Expr.NullsLast)
      }

prop_distinctOnExpr :: Orville.ConnectionPool -> HH.Property
prop_distinctOnExpr pool =
  orderByTest
    pool
    OrderByTest
      { orderByValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 1 "dog"]
      , orderByDistinctOn =
          Just . pure . RawSql.unsafeFromRawSql $ RawSql.toRawSql barColumn
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.appendOrderByExpr
              (Expr.orderByColumnName barColumn Expr.ascendingOrder)
              (Expr.orderByColumnName fooColumn Expr.ascendingOrder)
      }

data OrderByTest = OrderByTest
  { orderByValuesToInsert :: NE.NonEmpty FooBar
  , orderByClause :: Maybe Expr.OrderByClause
  , orderByDistinctOn :: Maybe (NE.NonEmpty Expr.DistinctOnExpr)
  , orderByExpectedQueryResults :: [FooBar]
  }

orderByTest :: Orville.ConnectionPool -> OrderByTest -> HH.Property
orderByTest pool test =
  Property.singletonProperty $ do
    rows <-
      MIO.liftIO $ do
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ orderByValuesToInsert test) Nothing Nothing

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectClause . Expr.selectDistinctOnExpr $ orderByDistinctOn test)
                (Expr.selectColumns [fooColumn, barColumn])
                ( Just $
                    Expr.tableExpr
                      (Expr.singleTableReferenceList fooBarTable)
                      Nothing
                      Nothing
                      (orderByClause test)
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                      Nothing
                )

          Execution.readRows result

    assertEqualFooBarRows rows (orderByExpectedQueryResults test)
