{-# LANGUAGE OverloadedStrings #-}

module Test.Expr.TSVector
  ( tsVectorTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

tsVectorTests :: Orville.ConnectionPool -> Property.Group
tsVectorTests pool =
  Property.group "Expr - TSVector" $
    [ prop_matchesOneRow pool
    , prop_toTSRank pool
    , prop_plainToTSQuery pool
    , prop_setTSWeight pool
    ]

prop_matchesOneRow :: Property.NamedDBProperty
prop_matchesOneRow =
  tsVectorTest "TSVector @@ TSQuery finds one result" $
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , tsVectorExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tsMatch
              ( Expr.toTSVector
                  barColumnRef
                  Nothing
              )
              ( Expr.toTSQuery
                  (Expr.valueExpression $ SqlValue.fromText ("bee" :: Text))
                  Nothing
              )
      , orderByClause = Nothing
      }

prop_setTSWeight :: Property.NamedDBProperty
prop_setTSWeight =
  tsVectorTest "Sets weight on a TSVector" $
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "weighted"]
      , tsVectorExpectedQueryResults = [mkFooBar 1 "weighted"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tsMatch
              ( Expr.setTSWeight
                  (Expr.toTSVector barColumnRef Nothing)
                  Expr.tsWeightA
              )
              (Expr.toTSQuery (Expr.valueExpression $ SqlValue.fromText ("weighted" :: Text)) Nothing)
      , orderByClause = Nothing
      }

prop_plainToTSQuery :: Property.NamedDBProperty
prop_plainToTSQuery =
  tsVectorTest "Creates TSQuery from plain text" $
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "plain query"]
      , tsVectorExpectedQueryResults = [mkFooBar 1 "plain query"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tsMatch
              (Expr.toTSVector barColumnRef Nothing)
              (Expr.plainToTSQuery (Expr.valueExpression $ SqlValue.fromText ("plain query" :: Text)) Nothing)
      , orderByClause = Nothing
      }

prop_toTSRank :: Property.NamedDBProperty
prop_toTSRank =
  tsVectorTest "Calculates rank of TSVector and TSQuery" $
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "foo", mkFooBar 2 "bar"]
      , tsVectorExpectedQueryResults = [mkFooBar 2 "bar", mkFooBar 1 "foo"]
      , whereClause = Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByValueExpression
              ( Expr.tsRankToValueExpression $
                  Expr.toTSRank
                    ( Expr.setTSWeight
                        (Expr.toTSVector barColumnRef Nothing)
                        Expr.tsWeightA
                    )
                    (Expr.toTSQuery (Expr.valueExpression $ SqlValue.fromText ("bar" :: Text)) Nothing)
              )
              Expr.descendingOrder
      }

data TSVectorTest = TSVectorTest
  { tsVectorValuesToInsert :: NE.NonEmpty FooBar
  , whereClause :: Maybe Expr.WhereClause
  , orderByClause :: Maybe Expr.OrderByClause
  , tsVectorExpectedQueryResults :: [FooBar]
  }

tsVectorTest :: String -> TSVectorTest -> Property.NamedDBProperty
tsVectorTest testName test =
  Property.singletonNamedDBProperty testName $ \pool -> do
    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection

          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource $ tsVectorValuesToInsert test) Nothing Nothing

          result <-
            RawSql.execute connection $
              Expr.queryExpr
                (Expr.selectClause $ Expr.selectExpr Nothing)
                (Expr.selectColumns [fooColumn, barColumn])
                (Just $ Expr.tableExpr (Expr.tableFromItem fooBarTable) (whereClause test) Nothing (orderByClause test) Nothing Nothing Nothing Nothing Nothing)

          Execution.readRows result

    assertEqualFooBarRows rows (tsVectorExpectedQueryResults test)
