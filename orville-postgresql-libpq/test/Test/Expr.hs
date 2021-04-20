module Test.Expr
  ( exprSpecs
  ) where


import qualified Data.ByteString.Char8 as B8
import           Data.Int (Int32)
import           Data.Pool (Pool)

import           Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as ExecResult
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue
import           Test.Tasty.Hspec (Spec, describe, it, shouldBe)

exprSpecs :: Pool Connection -> Spec
exprSpecs pool =
  describe "Expr Tests" $ do
    describe "WhereClause" $ do
      it "Returns all rows when where clause is specified" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [1,2,3]
            , whereClause = Nothing
            }

      it "equalsOp matches exact value" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [2]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.comparison
                    (Expr.columnReference fooColumn)
                    Expr.equalsOp
                    (Expr.comparisonValue (SqlValue.fromInt32 2))
            }

      it "greaterThanOp matches greater values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [3]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.comparison
                    (Expr.columnReference fooColumn)
                    Expr.greaterThanOp
                    (Expr.comparisonValue (SqlValue.fromInt32 2))
            }

      it "greaterThanOrEqualsOp matches greater or equal values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [2,3]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.comparison
                    (Expr.columnReference fooColumn)
                    Expr.greaterThanOrEqualsOp
                    (Expr.comparisonValue (SqlValue.fromInt32 2))
            }

      it "lessThanOp matches lesser values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [1]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.comparison
                    (Expr.columnReference fooColumn)
                    Expr.lessThanOp
                    (Expr.comparisonValue (SqlValue.fromInt32 2))
            }

      it "lessThanOrEqualsOp matches lesser or equal values" $ do
        runWhereConditionTest pool $
          WhereConditionTest
            { valuesToInsert = [1,2,3]
            , expectedQueryResults = [1,2]
            , whereClause =
                Just . Expr.whereClause $
                  Expr.comparison
                    (Expr.columnReference fooColumn)
                    Expr.lessThanOrEqualsOp
                    (Expr.comparisonValue (SqlValue.fromInt32 2))
            }

data WhereConditionTest =
  WhereConditionTest
    { valuesToInsert       :: [Int32]
    , whereClause          :: Maybe Expr.WhereClause
    , expectedQueryResults :: [Int32]
    }

mkTestInsertSource :: WhereConditionTest -> Expr.InsertSource
mkTestInsertSource test =
  let
    mkRow n = [SqlValue.fromInt32 n]
  in
    Expr.insertSqlValues (map mkRow $ valuesToInsert test)

mkTestExpectedRows :: WhereConditionTest -> [[(Maybe B8.ByteString, SqlValue)]]
mkTestExpectedRows test =
  let
    mkRow n = [(Just (B8.pack "foo"), SqlValue.fromInt32 n)]
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
        (Expr.selectColumns [fooColumn])
        (Expr.tableExpr exprTestTable (whereClause test))

  rows <- traverse ExecResult.readRows result
  rows `shouldBe` Just (mkTestExpectedRows test)

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "expr_test"

fooColumn :: Expr.ColumnName
fooColumn =
  Expr.rawColumnName "foo"

dropAndRecreateTestTable :: Pool Connection -> IO ()
dropAndRecreateTestTable pool = do
  RawSql.executeVoid pool (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)
  RawSql.executeVoid pool (RawSql.fromString "CREATE TABLE " <> Expr.tableNameToSql testTable <> RawSql.fromString "(foo INTEGER)")

