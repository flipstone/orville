module Test.Expr.OrderBy
  ( orderByTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.List.NonEmpty as NE
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

import Test.Expr.TestSchema (FooBar (..), assertEqualSqlRows, barColumn, dropAndRecreateTestTable, encodeFooBar, fooBarTable, fooColumn, insertFooBarSource)
import qualified Test.Property as Property

orderByTests :: Pool.Pool Conn.Connection -> Property.Group
orderByTests pool =
  Property.group "Expr - OrderBy" $
    [
      ( String.fromString "ascendingExpr sorts a text column"
      , runOrderByTest pool $
          OrderByTest
            { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dog"]
            , orderByClause =
                Just . Expr.orderByClause $
                  Expr.orderByExpr
                    (RawSql.toRawSql barColumn)
                    Expr.ascendingOrder
            }
      )
    ,
      ( String.fromString "descendingExpr sorts a text column"
      , runOrderByTest pool $
          OrderByTest
            { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , orderByExpectedQueryResults = [FooBar 1 "dog", FooBar 3 "dog", FooBar 2 "dingo"]
            , orderByClause =
                Just . Expr.orderByClause $
                  Expr.orderByExpr
                    (RawSql.toRawSql barColumn)
                    Expr.descendingOrder
            }
      )
    ,
      ( String.fromString "appendOrderByExpr causes ordering on both columns"
      , runOrderByTest pool $
          OrderByTest
            { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog", FooBar 1 "dog"]
            , orderByClause =
                Just . Expr.orderByClause $
                  Expr.appendOrderByExpr
                    (Expr.orderByExpr (RawSql.toRawSql barColumn) Expr.ascendingOrder)
                    (Expr.orderByExpr (RawSql.toRawSql fooColumn) Expr.descendingOrder)
            }
      )
    ,
      ( String.fromString "orderByColumnsExpr orders by columns"
      , runOrderByTest pool $
          OrderByTest
            { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog", FooBar 1 "dog"]
            , orderByClause =
                Just . Expr.orderByClause $
                  Expr.orderByColumnsExpr $
                    (barColumn, Expr.ascendingOrder)
                      NE.:| [(fooColumn, Expr.descendingOrder)]
            }
      )
    ]

data OrderByTest = OrderByTest
  { orderByValuesToInsert :: [FooBar]
  , orderByClause :: Maybe Expr.OrderByClause
  , orderByExpectedQueryResults :: [FooBar]
  }

runOrderByTest :: Pool.Pool Conn.Connection -> OrderByTest -> HH.Property
runOrderByTest pool test =
  Property.singletonProperty $ do
    rows <-
      MIO.liftIO $ do
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ orderByValuesToInsert test) Nothing

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectClause $ Expr.selectExpr Nothing)
                (Expr.selectColumns [fooColumn, barColumn])
                (Just $ Expr.tableExpr fooBarTable Nothing (orderByClause test) Nothing Nothing Nothing)

          ExecResult.readRows result

    rows `assertEqualSqlRows` map encodeFooBar (orderByExpectedQueryResults test)
