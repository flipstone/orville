{-# LANGUAGE OverloadedStrings #-}

module Test.Expr.TextSearch
  ( textSearchTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (FooBar (..), assertEqualFooBarRows, barColumn, barColumnRef, dropAndRecreateTestTable, fooBarTable, fooColumn, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

textSearchTests :: Orville.ConnectionPool -> Tasty.TestTree
textSearchTests pool =
  Tasty.testGroup
    "Expr - TSVector"
    [ TastyHH.testProperty "TSVector @@ TSQuery finds one result" (prop_matchesOneRow pool)
    , TastyHH.testProperty "Calculates rank of TSVector and TSQuery" (prop_toTSRank pool)
    , TastyHH.testProperty "Creates TSQuery from plain text" (prop_plainToTSQuery pool)
    , TastyHH.testProperty "Sets weight on a TSVector" (prop_setTSWeight pool)
    ]

prop_matchesOneRow :: Orville.ConnectionPool -> HH.Property
prop_matchesOneRow pool =
  tsVectorTest
    pool
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "ant", mkFooBar 2 "bee", mkFooBar 3 "chihuahua"]
      , tsVectorExpectedQueryResults = [mkFooBar 2 "bee"]
      , whereClause =
          Just . Expr.whereClause $
            Expr.tsMatch
              ( Expr.toTSVector
                  barColumnRef
                  (Just Expr.englishRegConfig)
              )
              ( Expr.toTSQuery
                  (Expr.valueExpression $ SqlValue.fromText ("bee" :: Text))
                  Nothing
              )
      , orderByClause = Nothing
      }

prop_setTSWeight :: Orville.ConnectionPool -> HH.Property
prop_setTSWeight pool =
  tsVectorTest
    pool
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

prop_plainToTSQuery :: Orville.ConnectionPool -> HH.Property
prop_plainToTSQuery pool =
  tsVectorTest
    pool
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

prop_toTSRank :: Orville.ConnectionPool -> HH.Property
prop_toTSRank pool =
  tsVectorTest
    pool
    TSVectorTest
      { tsVectorValuesToInsert = NE.fromList [mkFooBar 1 "foo", mkFooBar 2 "bar"]
      , tsVectorExpectedQueryResults = [mkFooBar 2 "bar", mkFooBar 1 "foo"]
      , whereClause = Nothing
      , orderByClause =
          Just . Expr.orderByClause $
            Expr.orderByValueExpression
              ( Expr.toTSRank
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

tsVectorTest :: Orville.ConnectionPool -> TSVectorTest -> HH.Property
tsVectorTest pool test =
  Property.singletonProperty $ do
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
                (Just $ Expr.tableExpr (Expr.singleTableReferenceList fooBarTable) (whereClause test) Nothing (orderByClause test) Nothing Nothing Nothing Nothing Nothing)

          Execution.readRows result

    assertEqualFooBarRows rows (tsVectorExpectedQueryResults test)
