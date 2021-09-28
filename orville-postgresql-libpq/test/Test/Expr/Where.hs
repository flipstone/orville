module Test.Expr.Where
  ( whereTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Pool as Pool
import qualified Data.Text as T

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualSqlRows, barColumn, dropAndRecreateTestTable, encodeFooBar, fooBarTable, fooColumn, insertFooBarSource)
import qualified Test.Property as Property

whereTests :: Pool.Pool Connection.Connection -> Property.Group
whereTests pool =
  Property.group "Expr - WhereClause" $
    [ prop_noWhereClauseSpecified pool
    , prop_equalsOp pool
    , prop_greaterThanOp pool
    , prop_greaterThanOrEqualsOp pool
    , prop_lessThanOp pool
    , prop_lessThanOrEqualsToOp pool
    , prop_andExpr pool
    , prop_orExpr pool
    , prop_columnIn pool
    , prop_columnNotIn pool
    , prop_columnTupleIn pool
    , prop_columnTupleNotIn pool
    ]

prop_noWhereClauseSpecified :: Property.NamedDBProperty
prop_noWhereClauseSpecified =
  whereConditionTest "Returns all rows when where clause is specified" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereClause = Nothing
      }

prop_equalsOp :: Property.NamedDBProperty
prop_equalsOp =
  whereConditionTest "equalsOp matches exact value" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnEquals fooColumn (SqlValue.fromInt32 2)
      }

prop_greaterThanOp :: Property.NamedDBProperty
prop_greaterThanOp =
  whereConditionTest "greaterThanOp matches greater values" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnGreaterThan fooColumn (SqlValue.fromInt32 2)
      }

prop_greaterThanOrEqualsOp :: Property.NamedDBProperty
prop_greaterThanOrEqualsOp =
  whereConditionTest "greaterThanOrEqualsOp matches greater or equal values" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnGreaterThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
      }

prop_lessThanOp :: Property.NamedDBProperty
prop_lessThanOp =
  whereConditionTest "lessThanOp matches lesser values" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 1 "ant"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnLessThan fooColumn (SqlValue.fromInt32 2)
      }

prop_lessThanOrEqualsToOp :: Property.NamedDBProperty
prop_lessThanOrEqualsToOp =
  whereConditionTest "lessThanOrEqualsOp matches lesser or equal values" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnLessThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
      }

prop_andExpr :: Property.NamedDBProperty
prop_andExpr =
  whereConditionTest "andExpr requires both conditions to be true" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.andExpr
              (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
              (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))
      }

prop_orExpr :: Property.NamedDBProperty
prop_orExpr =
  whereConditionTest "orExpr requires either conditions to be true" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.orExpr
              (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
              (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dingo")))
      }

prop_columnIn :: Property.NamedDBProperty
prop_columnIn =
  whereConditionTest "columnIn requires the column's value to be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 1 "dog", FooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnIn
              barColumn
              (SqlValue.fromText (T.pack "dog") :| [SqlValue.fromText (T.pack "cat")])
      }

prop_columnNotIn :: Property.NamedDBProperty
prop_columnNotIn =
  whereConditionTest "columnNotIn requires the column's value to not be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnNotIn
              barColumn
              (SqlValue.fromText (T.pack "dog") :| [SqlValue.fromText (T.pack "cat")])
      }

prop_columnTupleIn :: Property.NamedDBProperty
prop_columnTupleIn =
  whereConditionTest "columnTupleIn requires the column value combination to be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 1 "dog", FooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnTupleIn
              (fooColumn :| [barColumn])
              ( (SqlValue.fromInt32 1 :| [SqlValue.fromText (T.pack "dog")])
                  :| [SqlValue.fromInt32 2 :| [SqlValue.fromText (T.pack "dingo")]]
              )
      }

prop_columnTupleNotIn :: Property.NamedDBProperty
prop_columnTupleNotIn =
  whereConditionTest "columnTupleNotIn requires the column value combination to not be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
      , whereExpectedQueryResults = [FooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.columnTupleNotIn
              (fooColumn :| [barColumn])
              ( (SqlValue.fromInt32 1 :| [SqlValue.fromText (T.pack "dog")])
                  :| [SqlValue.fromInt32 2 :| [SqlValue.fromText (T.pack "dingo")]]
              )
      }

data WhereConditionTest = WhereConditionTest
  { whereValuesToInsert :: [FooBar]
  , whereClause :: Maybe Expr.WhereClause
  , whereExpectedQueryResults :: [FooBar]
  }

whereConditionTest :: String -> WhereConditionTest -> Property.NamedDBProperty
whereConditionTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <-
      MIO.liftIO $
        Pool.withResource pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ whereValuesToInsert test) Nothing

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectClause $ Expr.selectExpr Nothing)
                (Expr.selectColumns [fooColumn, barColumn])
                (Just $ Expr.tableExpr fooBarTable (whereClause test) Nothing Nothing Nothing Nothing)

          ExecResult.readRows result

    rows `assertEqualSqlRows` map encodeFooBar (whereExpectedQueryResults test)
