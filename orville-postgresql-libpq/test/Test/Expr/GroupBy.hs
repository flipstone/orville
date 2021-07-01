module Test.Expr.GroupBy
  ( groupByTests,
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

groupByTests :: Pool.Pool Conn.Connection -> IO Bool
groupByTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "Expr - GroupBy")
      [
        ( String.fromString "appendGroupBy causes grouping on both clauses"
        , runGroupByTest pool $
            GroupByTest
              { groupByValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 1 "dog", FooBar 3 "dingo", FooBar 1 "dog", FooBar 2 "dingo"]
              , groupByExpectedQueryResults = [FooBar 1 "dog", FooBar 3 "dingo", FooBar 2 "dingo"]
              , groupByClause =
                  Just . Expr.groupByClause $
                    Expr.appendGroupBy
                      (Expr.groupByExpr $ Expr.columnNameToSql barColumn)
                      (Expr.groupByExpr $ Expr.columnNameToSql fooColumn)
              }
        )
      ]

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

mkGroupByTestExpectedRows :: GroupByTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkGroupByTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (groupByExpectedQueryResults test)

runGroupByTest :: Pool.Pool Conn.Connection -> GroupByTest -> HH.Property
runGroupByTest pool test = Property.singletonProperty $
  Pool.withResource pool $ \connection -> do
    MIO.liftIO $ dropAndRecreateTestTable connection

    let exprTestTable = Expr.rawTableName "expr_test"

    MIO.liftIO . RawSql.executeVoid connection
      . Expr.insertExprToSql
      $ Expr.insertExpr exprTestTable Nothing (mkGroupByTestInsertSource test)

    result <-
      MIO.liftIO $
        RawSql.execute connection
          . Expr.queryExprToSql
          $ Expr.queryExpr
            (Expr.selectColumns [fooColumn, barColumn])
            (Expr.tableExpr exprTestTable Nothing Nothing (groupByClause test))

    rows <- MIO.liftIO $ ExecResult.readRows result
    rows HH.=== mkGroupByTestExpectedRows test

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
