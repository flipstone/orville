module Test.Expr.Where
  ( whereTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, fooBarTable, fooColumn, fooColumnRef, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

whereTests :: Orville.ConnectionPool -> Property.Group
whereTests pool =
  Property.group "Expr - WhereClause" $
    [ prop_noWhereClauseSpecified pool
    , prop_equalsOp pool
    , prop_isDistinctFromOp pool
    , prop_isNotDistinctFromOp pool
    , prop_greaterThanOp pool
    , prop_greaterThanOrEqualsOp pool
    , prop_lessThanOp pool
    , prop_lessThanOrEqualsToOp pool
    , prop_andExpr pool
    , prop_orExpr pool
    , prop_valueIn pool
    , prop_valueNotIn pool
    , prop_tupleIn pool
    , prop_tupleNotIn pool
    ]

prop_noWhereClauseSpecified :: Property.NamedDBProperty
prop_noWhereClauseSpecified =
  whereConditionTest "Returns all rows when where clause is specified" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereClause = Nothing
      }

prop_equalsOp :: Property.NamedDBProperty
prop_equalsOp =
  whereConditionTest "equalsOp matches exact value" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.equals fooColumnRef (int32ValueExpr 2)
      }

prop_isDistinctFromOp :: Property.NamedDBProperty
prop_isDistinctFromOp =
  whereConditionTest "isDistinctFromOp matches on null correctly" $
    WhereConditionTest
      { whereValuesToInsert = [FooBar Nothing (Just "ant"), mkFooBar 2 "bee", FooBar Nothing (Just "chihuahua")]
      , whereExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.isDistinctFrom fooColumnRef (Expr.valueExpression SqlValue.sqlNull)
      }

prop_isNotDistinctFromOp :: Property.NamedDBProperty
prop_isNotDistinctFromOp =
  whereConditionTest "isNotDistinctFromOp matches on null correctly" $
    let
      expectedResults = [FooBar Nothing (Just "ant"), FooBar Nothing (Just "chihuahua")]
    in
      WhereConditionTest
        { whereValuesToInsert = [mkFooBar 2 "bee"] <> expectedResults
        , whereExpectedQueryResults = expectedResults
        , whereClause =
            Just . Expr.whereClause $
              Expr.isNotDistinctFrom fooColumnRef (Expr.valueExpression SqlValue.sqlNull)
        }

prop_greaterThanOp :: Property.NamedDBProperty
prop_greaterThanOp =
  whereConditionTest "greaterThanOp matches greater values" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.greaterThan fooColumnRef (int32ValueExpr 2)
      }

prop_greaterThanOrEqualsOp :: Property.NamedDBProperty
prop_greaterThanOrEqualsOp =
  whereConditionTest "greaterThanOrEqualsOp matches greater or equal values" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.greaterThanOrEqualTo fooColumnRef (int32ValueExpr 2)
      }

prop_lessThanOp :: Property.NamedDBProperty
prop_lessThanOp =
  whereConditionTest "lessThanOp matches lesser values" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.lessThan fooColumnRef (int32ValueExpr 2)
      }

prop_lessThanOrEqualsToOp :: Property.NamedDBProperty
prop_lessThanOrEqualsToOp =
  whereConditionTest "lessThanOrEqualsOp matches lesser or equal values" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant", mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.lessThanOrEqualTo fooColumnRef (int32ValueExpr 2)
      }

prop_andExpr :: Property.NamedDBProperty
prop_andExpr =
  whereConditionTest "andExpr requires both conditions to be true" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.andExpr
              (Expr.equals fooColumnRef (int32ValueExpr 3))
              (Expr.equals barColumnRef (textValueExpr "dog"))
      }

prop_orExpr :: Property.NamedDBProperty
prop_orExpr =
  whereConditionTest "orExpr requires either conditions to be true" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.orExpr
              (Expr.equals fooColumnRef (int32ValueExpr 3))
              (Expr.equals barColumnRef (textValueExpr "dingo"))
      }

prop_valueIn :: Property.NamedDBProperty
prop_valueIn =
  whereConditionTest "valueIn requires the column's value to be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.valueIn
              barColumnRef
              (textValueExpr "dog" :| [textValueExpr "cat"])
      }

prop_valueNotIn :: Property.NamedDBProperty
prop_valueNotIn =
  whereConditionTest "valueNotIn requires the column's value to not be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.valueNotIn
              barColumnRef
              (textValueExpr "dog" :| [textValueExpr "cat"])
      }

prop_tupleIn :: Property.NamedDBProperty
prop_tupleIn =
  whereConditionTest "tupleIn requires the column value combination to be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tupleIn
              (fooColumnRef :| [barColumnRef])
              ( (int32ValueExpr 1 :| [textValueExpr "dog"])
                  :| [int32ValueExpr 2 :| [textValueExpr "dingo"]]
              )
      }

prop_tupleNotIn :: Property.NamedDBProperty
prop_tupleNotIn =
  whereConditionTest "tupleNotIn requires the column value combination to not be in the list" $
    WhereConditionTest
      { whereValuesToInsert = [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tupleNotIn
              (fooColumnRef :| [barColumnRef])
              ( (int32ValueExpr 1 :| [textValueExpr "dog"])
                  :| [int32ValueExpr 2 :| [textValueExpr "dingo"]]
              )
      }

int32ValueExpr :: Int32 -> Expr.ValueExpression
int32ValueExpr =
  Expr.valueExpression . SqlValue.fromInt32

textValueExpr :: String -> Expr.ValueExpression
textValueExpr =
  Expr.valueExpression . SqlValue.fromText . T.pack

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
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ whereValuesToInsert test) Nothing Nothing

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectClause $ Expr.selectExpr Nothing)
                (Expr.selectColumns [fooColumn, barColumn])
                (Just $ Expr.tableExpr (Expr.referencesTable fooBarTable) (whereClause test) Nothing Nothing Nothing Nothing)

          Execution.readRows result

    assertEqualFooBarRows rows (whereExpectedQueryResults test)
