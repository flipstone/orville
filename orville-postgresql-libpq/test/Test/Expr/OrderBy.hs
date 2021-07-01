module Test.Expr.OrderBy
  ( orderByTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Database.Orville.PostgreSQL.Connection as Conn
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

orderByTests :: Pool.Pool Conn.Connection -> IO Bool
orderByTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "Expr - OrderBy")
      [
        ( String.fromString "ascendingExpr sorts a text column"
        , runOrderByTest pool $
            OrderByTest
              { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
              , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dog"]
              , orderByClause =
                  Just . Expr.orderByClause $
                    Expr.orderByExpr
                      (Expr.columnNameToSql barColumn)
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
                      (Expr.columnNameToSql barColumn)
                      Expr.descendingOrder
              }
        )
      ,
        ( String.fromString "addOrderBy causes ordering on both columns"
        , runOrderByTest pool $
            OrderByTest
              { orderByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
              , orderByExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog", FooBar 1 "dog"]
              , orderByClause =
                  Just . Expr.orderByClause $
                    Expr.appendOrderBy
                      (Expr.orderByExpr (Expr.columnNameToSql barColumn) Expr.ascendingOrder)
                      (Expr.orderByExpr (Expr.columnNameToSql fooColumn) Expr.descendingOrder)
              }
        )
      ]

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

mkOrderByTestExpectedRows :: OrderByTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkOrderByTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (orderByExpectedQueryResults test)

runOrderByTest :: Pool.Pool Conn.Connection -> OrderByTest -> HH.Property
runOrderByTest pool test = Property.singletonProperty $
  Pool.withResource pool $ \connection -> do
    MIO.liftIO $ dropAndRecreateTestTable connection

    let exprTestTable = Expr.rawTableName "expr_test"

    MIO.liftIO . RawSql.executeVoid connection
      . Expr.insertExprToSql
      $ Expr.insertExpr exprTestTable Nothing (mkOrderByTestInsertSource test)

    result <-
      MIO.liftIO $
        RawSql.execute connection
          . Expr.queryExprToSql
          $ Expr.queryExpr
            (Expr.selectColumns [fooColumn, barColumn])
            (Expr.tableExpr exprTestTable Nothing (orderByClause test) Nothing)

    rows <- MIO.liftIO $ ExecResult.readRows result
    rows HH.=== mkOrderByTestExpectedRows test

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "expr_test"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.rawColumnName "bar"

dropAndRecreateTestTable :: Conn.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> Expr.tableNameToSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
