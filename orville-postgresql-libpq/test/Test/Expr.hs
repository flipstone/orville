module Test.Expr
  ( exprSpecs
  ) where


import qualified Data.ByteString.Char8 as B8
import           Data.Int (Int32)
import           Data.Pool (Pool)
import qualified Data.Text as T

import           Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)

data FooBar =
  FooBar
    { foo :: Int32
    , bar :: String
    }

exprSpecs :: Pool Connection -> Spec
exprSpecs pool =
  describe "Expr Tests" $ do
    describe "WhereClause" $ do
      it "Returns all rows when where clause is specified" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , whereClause = Nothing
            }

      it "equalsOp matches exact value" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 2 "bee"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.columnEquals fooColumn (SqlValue.fromInt32 2)
            }

      it "greaterThanOp matches greater values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 3 "chihuahua"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.columnGreaterThan fooColumn (SqlValue.fromInt32 2)
            }

      it "greaterThanOrEqualsOp matches greater or equal values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 2 "bee", FooBar 3 "chihuahua"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.columnGreaterThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
            }

      it "lessThanOp matches lesser values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 1 "ant"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.columnLessThan fooColumn (SqlValue.fromInt32 2)
            }

      it "lessThanOrEqualsOp matches lesser or equal values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "ant", FooBar 2 "bee", FooBar 3 "chihuahua"]
            , expectedQueryResults = [FooBar 1 "ant", FooBar 2 "bee"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.columnLessThanOrEqualTo fooColumn (SqlValue.fromInt32 2)
            }

      it "andExpr requires both conditions to be true" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , expectedQueryResults = [FooBar 3 "dog"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.andExpr
                    (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                    (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dog")))
            }

      it "orExpr requires either conditions to be true" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [FooBar 1 "dog", FooBar 2 "dingo", FooBar 3 "dog"]
            , expectedQueryResults = [FooBar 2 "dingo", FooBar 3 "dog"]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.orExpr
                    (Expr.columnEquals fooColumn (SqlValue.fromInt32 3))
                    (Expr.columnEquals barColumn (SqlValue.fromText (T.pack "dingo")))
            }

data WhereConditionTest =
  WhereConditionTest
    { valuesToInsert       :: [FooBar]
    , whereClause          :: Maybe Expr.WhereClause
    , expectedQueryResults :: [FooBar]
    }

mkTestInsertSource :: WhereConditionTest -> Expr.InsertSource
mkTestInsertSource test =
  let
    mkRow foobar =
      [ SqlValue.fromInt32 (foo foobar)
      , SqlValue.fromText (T.pack $ bar foobar)
      ]
  in
    Expr.insertSqlValues (map mkRow $ valuesToInsert test)

mkTestExpectedRows :: WhereConditionTest -> [[(Maybe B8.ByteString, SqlValue)]]
mkTestExpectedRows test =
  let
    mkRow foobar =
      [ (Just (B8.pack "foo"), SqlValue.fromInt32 (foo foobar))
      , (Just (B8.pack "bar"), SqlValue.fromText (T.pack $ bar foobar))
      ]
  in
    map mkRow (expectedQueryResults test)

runWhereConditionTest :: Pool Connection -> WhereConditionTest -> IO ()
runWhereConditionTest pool test = do
  dropAndRecreateTestTable pool

  let
    exprTestTable = Expr.rawTableName "expr_test"

  RawSql.executeVoid pool $
    Expr.insertExprToSql $
      Expr.insertExpr exprTestTable (mkTestInsertSource test)

  result <- RawSql.execute pool $
    Expr.queryExprToSql $
      Expr.queryExpr
        (Expr.selectColumns [fooColumn, barColumn])
        (Expr.tableExpr exprTestTable (whereClause test))

  rows <- traverse ExecResult.readRows result
  rows `shouldBe` Just (mkTestExpectedRows test)

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "expr_test"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

barColumn :: Expr.ColumnName
barColumn =
  Expr.rawColumnName "bar"

dropAndRecreateTestTable :: Pool Connection -> IO ()
dropAndRecreateTestTable pool = do
  RawSql.executeVoid pool (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)
  RawSql.executeVoid pool (RawSql.fromString "CREATE TABLE " <> Expr.tableNameToSql testTable <> RawSql.fromString "(foo INTEGER, bar TEXT)")

