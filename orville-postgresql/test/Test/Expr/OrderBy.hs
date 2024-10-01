module Test.Expr.OrderBy
  ( orderByTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.List.NonEmpty as NE

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, dropAndRecreateTestTable, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

orderByTests :: Orville.ConnectionPool -> Property.Group
orderByTests pool =
  Property.group
    "Expr - OrderBy"
    [ prop_ascendingExpr pool
    , prop_descendingExpr pool
    , prop_appendOrderByExpr pool
    , prop_orderByColumnsExpr pool
    , prop_ascendingOrderWithExpr pool
    , prop_descendingOrderWithExpr pool
    , prop_distinctOnExpr pool
    ]

prop_ascendingExpr :: Property.NamedDBProperty
prop_ascendingExpr =
  orderByTest "ascendingExpr sorts a text column" $
    OrderByTest
      { orderByValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 1 "dog", mkFooBar 3 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnName barColumn Expr.ascendingOrder
      }

prop_descendingExpr :: Property.NamedDBProperty
prop_descendingExpr =
  orderByTest "descendingExpr sorts a text column" $
    OrderByTest
      { orderByValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 3 "dog", mkFooBar 2 "dingo"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnName barColumn Expr.descendingOrder
      }

prop_appendOrderByExpr :: Property.NamedDBProperty
prop_appendOrderByExpr =
  orderByTest "appendOrderByExpr causes ordering on both columns" $
    OrderByTest
      { orderByValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog", mkFooBar 1 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.appendOrderByExpr
              (Expr.orderByColumnName barColumn Expr.ascendingOrder)
              (Expr.orderByColumnName fooColumn Expr.descendingOrder)
      }

prop_orderByColumnsExpr :: Property.NamedDBProperty
prop_orderByColumnsExpr =
  orderByTest "orderByColumnsExpr orders by columns" $
    OrderByTest
      { orderByValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , orderByExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog", mkFooBar 1 "dog"]
      , orderByDistinctOn =
          Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByColumnsExpr $
              (barColumn, Expr.ascendingOrder)
                NE.:| [(fooColumn, Expr.descendingOrder)]
      }

prop_ascendingOrderWithExpr :: Property.NamedDBProperty
prop_ascendingOrderWithExpr =
  orderByTest "ascendingOrderWith sorts columns with nulls first/last" $
    OrderByTest
      { orderByValuesToInsert =
          [FooBar Nothing Nothing, FooBar (Just 1) Nothing, mkFooBar 2 "dog", FooBar Nothing (Just "dog")]
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

prop_descendingOrderWithExpr :: Property.NamedDBProperty
prop_descendingOrderWithExpr =
  orderByTest "descendingOrderWith sorts columns with nulls first/last" $
    OrderByTest
      { orderByValuesToInsert =
          [FooBar Nothing Nothing, FooBar (Just 1) Nothing, mkFooBar 2 "dog", FooBar Nothing (Just "dog")]
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

prop_distinctOnExpr :: Property.NamedDBProperty
prop_distinctOnExpr =
  orderByTest "descendingExpr sorts a text column as expected with distinctOn" $
    OrderByTest
      { orderByValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
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
  { orderByValuesToInsert :: [FooBar]
  , orderByClause :: Maybe Expr.OrderByClause
  , orderByDistinctOn :: Maybe (NE.NonEmpty Expr.DistinctOnExpr)
  , orderByExpectedQueryResults :: [FooBar]
  }

orderByTest :: String -> OrderByTest -> Property.NamedDBProperty
orderByTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
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
                (Just $ Expr.tableExpr (Expr.tableFromItem fooBarTable) Nothing Nothing (orderByClause test) Nothing Nothing Nothing Nothing)

          Execution.readRows result

    assertEqualFooBarRows rows (orderByExpectedQueryResults test)
