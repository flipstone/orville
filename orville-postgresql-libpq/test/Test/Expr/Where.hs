module Test.Expr.Where
  ( whereSpecs,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Database.Orville.PostgreSQL.Connection as Connection
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

data FooBar = FooBar
  { foo :: Int.Int32
  , bar :: String
  }

whereSpecs :: Pool.Pool Connection.Connection -> IO Bool
whereSpecs pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "Expr - WhereClause")
      [
        ( String.fromString "Returns all rows when where clause is specified"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereClause = Nothing
              }
        )
      ,
        ( String.fromString "equalsOp matches exact value"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 2 "bee"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.columnEquals fooColumn (SqlValue.fromInt32 2)
              }
        )
      ,
        ( String.fromString "greaterThanOp matches greater values"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 3 "chihuahua"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.columnGreaterThan fooColumn (SqlValue.fromInt32 2)
              }
        )
      ,
        ( String.fromString "greaterThanOrEqualsOp matches greater or equal values"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.columnGreaterThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
              }
        )
      ,
        ( String.fromString "lessThanOp matches lesser values"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 1 "ant"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.columnLessThan fooColumn (SqlValue.fromInt32 2)
              }
        )
      ,
        ( String.fromString "lessThanOrEqualsOp matches lesser or equal values"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
              , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.columnLessThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
              }
        )
      ,
        ( String.fromString "andExpr requires both conditions to be true"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
              , whereExpectedQueryResults = [FooBar 3 "dog"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.andExpr
                      (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                      (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))
              }
        )
      ,
        ( String.fromString "orExpr requires either conditions to be true"
        , runWhereConditionTest pool $
            WhereConditionTest
              { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
              , whereExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog"]
              , whereClause =
                  Just . Expr.whereClause $
                    Expr.orExpr
                      (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                      (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dingo")))
              }
        )
      ]

data WhereConditionTest = WhereConditionTest
  { whereValuesToInsert :: [FooBar]
  , whereClause :: Maybe Expr.WhereClause
  , whereExpectedQueryResults :: [FooBar]
  }

mkTestInsertSource :: WhereConditionTest -> Expr.InsertSource
mkTestInsertSource test =
  let mkRow foobar =
        [ SqlValue.fromInt32 (foo foobar)
        , SqlValue.fromText (T.pack $ bar foobar)
        ]
   in Expr.insertSqlValues (map mkRow $ whereValuesToInsert test)

mkTestExpectedRows :: WhereConditionTest -> [[(Maybe B8.ByteString, SqlValue.SqlValue)]]
mkTestExpectedRows test =
  let mkRow foobar =
        [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
        , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
        ]
   in fmap mkRow (whereExpectedQueryResults test)

runWhereConditionTest :: Pool.Pool Connection.Connection -> WhereConditionTest -> HH.Property
runWhereConditionTest pool test =
  Property.singletonProperty $
    Pool.withResource pool $ \connection -> do
      MIO.liftIO $ dropAndRecreateTestTable connection

      let exprTestTable = Expr.rawTableName "expr_test"

      MIO.liftIO . RawSql.executeVoid connection $
        Expr.insertExprToSql $
          Expr.insertExpr exprTestTable Nothing (mkTestInsertSource test)

      result <-
        MIO.liftIO
          . RawSql.execute connection
          $ Expr.queryExprToSql $
            Expr.queryExpr
              (Expr.selectColumns [fooColumn, barColumn])
              (Expr.tableExpr exprTestTable (whereClause test) Nothing)

      rows <- MIO.liftIO $ ExecResult.readRows result
      rows HH.=== mkTestExpectedRows test

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "expr_test"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.rawColumnName "bar"

dropAndRecreateTestTable :: Connection.Connection -> IO ()
dropAndRecreateTestTable connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)
  RawSql.executeVoid connection (RawSql.fromString "CREATE TABLE " <> Expr.tableNameToSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")
