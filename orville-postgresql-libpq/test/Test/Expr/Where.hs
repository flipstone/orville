module Test.Expr.Where
  ( whereTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), barColumn, dropAndRecreateTestTable, encodeFooBar, fooBarTable, fooColumn, insertFooBarSource, sqlValuesToText)
import qualified Test.Property as Property

whereTests :: Pool.Pool Connection.Connection -> IO Bool
whereTests pool =
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

runWhereConditionTest :: Pool.Pool Connection.Connection -> WhereConditionTest -> HH.Property
runWhereConditionTest pool test =
  Property.singletonProperty $ do
    rows <-
      MIO.liftIO $
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ whereValuesToInsert test)

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectColumns [fooColumn, barColumn])
                (Expr.tableExpr fooBarTable (whereClause test) Nothing Nothing Nothing)

          ExecResult.readRows result

    sqlValuesToText rows HH.=== sqlValuesToText (map encodeFooBar (whereExpectedQueryResults test))
