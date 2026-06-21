module Test.Expr.Where
  ( whereTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, fooBarTable, fooColumn, fooColumnRef, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

whereTests :: Orville.ConnectionPool -> Tasty.TestTree
whereTests pool =
  Tasty.testGroup "Expr - WhereClause" $
    [ TastyHH.testProperty "Returns all rows when where clause is specified" (prop_noWhereClauseSpecified pool)
    , TastyHH.testProperty "equalsOp matches exact value" (prop_equalsOp pool)
    , TastyHH.testProperty "isDistinctFromOp matches on null correctly" (prop_isDistinctFromOp pool)
    , TastyHH.testProperty "isNotDistinctFromOp matches on null correctly" (prop_isNotDistinctFromOp pool)
    , TastyHH.testProperty "greaterThanOp matches greater values" (prop_greaterThanOp pool)
    , TastyHH.testProperty "greaterThanOrEqualsOp matches greater or equal values" (prop_greaterThanOrEqualsOp pool)
    , TastyHH.testProperty "lessThanOp matches lesser values" (prop_lessThanOp pool)
    , TastyHH.testProperty "lessThanOrEqualsOp matches lesser or equal values" (prop_lessThanOrEqualsToOp pool)
    , TastyHH.testProperty "andExpr requires both conditions to be true" (prop_andExpr pool)
    , TastyHH.testProperty "orExpr requires either conditions to be true" (prop_orExpr pool)
    , TastyHH.testProperty "notExpr inverts condition" (prop_notExpr pool)
    , TastyHH.testProperty "valueIn requires the column's value to be in the list" (prop_valueIn pool)
    , TastyHH.testProperty "valueNotIn requires the column's value to not be in the list" (prop_valueNotIn pool)
    , TastyHH.testProperty "tupleIn requires the column value combination to be in the list" (prop_tupleIn pool)
    , TastyHH.testProperty "tupleNotIn requires the column value combination to not be in the list" (prop_tupleNotIn pool)
    ]

prop_noWhereClauseSpecified :: Orville.ConnectionPool -> HH.Property
prop_noWhereClauseSpecified pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereClause = Nothing
      }

prop_equalsOp :: Orville.ConnectionPool -> HH.Property
prop_equalsOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.equals fooColumnRef (int32ValueExpr 2)
      }

prop_isDistinctFromOp :: Orville.ConnectionPool -> HH.Property
prop_isDistinctFromOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [FooBar Nothing (Just "ant"), mkFooBar 2 "bee", FooBar Nothing (Just "chihuahua")]
      , whereExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.isDistinctFrom fooColumnRef (Expr.valueExpression SqlValue.sqlNull)
      }

prop_isNotDistinctFromOp :: Orville.ConnectionPool -> HH.Property
prop_isNotDistinctFromOp pool =
  whereConditionTest pool $
    let
      expectedResults = [FooBar Nothing (Just "ant"), FooBar Nothing (Just "chihuahua")]
    in
      WhereConditionTest
        { whereValuesToInsert = mkFooBar 2 "bee" :| expectedResults
        , whereExpectedQueryResults = expectedResults
        , whereClause =
            Just . Expr.whereClause $
              Expr.isNotDistinctFrom fooColumnRef (Expr.valueExpression SqlValue.sqlNull)
        }

prop_greaterThanOp :: Orville.ConnectionPool -> HH.Property
prop_greaterThanOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.greaterThan fooColumnRef (int32ValueExpr 2)
      }

prop_greaterThanOrEqualsOp :: Orville.ConnectionPool -> HH.Property
prop_greaterThanOrEqualsOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.greaterThanOrEqualTo fooColumnRef (int32ValueExpr 2)
      }

prop_lessThanOp :: Orville.ConnectionPool -> HH.Property
prop_lessThanOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.lessThan fooColumnRef (int32ValueExpr 2)
      }

prop_lessThanOrEqualsToOp :: Orville.ConnectionPool -> HH.Property
prop_lessThanOrEqualsToOp pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , whereExpectedQueryResults = [mkFooBar 1 "ant", mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.lessThanOrEqualTo fooColumnRef (int32ValueExpr 2)
      }

prop_andExpr :: Orville.ConnectionPool -> HH.Property
prop_andExpr pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.andExpr
              (Expr.equals fooColumnRef (int32ValueExpr 3))
              (Expr.equals barColumnRef (textValueExpr "dog"))
      }

prop_orExpr :: Orville.ConnectionPool -> HH.Property
prop_orExpr pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.orExpr
              (Expr.equals fooColumnRef (int32ValueExpr 3))
              (Expr.equals barColumnRef (textValueExpr "dingo"))
      }

prop_notExpr :: Orville.ConnectionPool -> HH.Property
prop_notExpr pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause . Expr.notExpr $
            Expr.orExpr
              (Expr.equals fooColumnRef (int32ValueExpr 3))
              (Expr.equals barColumnRef (textValueExpr "dog"))
      }

prop_valueIn :: Orville.ConnectionPool -> HH.Property
prop_valueIn pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 3 "dog"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.valueIn
              barColumnRef
              (textValueExpr "dog" :| [textValueExpr "cat"])
      }

prop_valueNotIn :: Orville.ConnectionPool -> HH.Property
prop_valueNotIn pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.valueNotIn
              barColumnRef
              (textValueExpr "dog" :| [textValueExpr "cat"])
      }

prop_tupleIn :: Orville.ConnectionPool -> HH.Property
prop_tupleIn pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
      , whereExpectedQueryResults = [mkFooBar 1 "dog", mkFooBar 2 "dingo"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tupleIn
              (fooColumnRef :| [barColumnRef])
              ( (int32ValueExpr 1 :| [textValueExpr "dog"])
                  :| [int32ValueExpr 2 :| [textValueExpr "dingo"]]
              )
      }

prop_tupleNotIn :: Orville.ConnectionPool -> HH.Property
prop_tupleNotIn pool =
  whereConditionTest
    pool
    WhereConditionTest
      { whereValuesToInsert = NE.fromList [mkFooBar 1 "dog", mkFooBar 2 "dingo", mkFooBar 3 "dog"]
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
  { whereValuesToInsert :: NE.NonEmpty FooBar
  , whereClause :: Maybe Expr.WhereClause
  , whereExpectedQueryResults :: [FooBar]
  }

whereConditionTest :: Orville.ConnectionPool -> WhereConditionTest -> HH.Property
whereConditionTest pool test =
  Property.singletonProperty $ do
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
                (Just $ Expr.tableExpr (Expr.singleTableReferenceList fooBarTable) (whereClause test) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

          Execution.readRows result

    assertEqualFooBarRows rows (whereExpectedQueryResults test)
