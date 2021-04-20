module Test.SqlType
  ( sqlTypeSpecs
  ) where

import Data.Pool (Pool)
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Time as Time
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, expectationFailure)

import Database.Orville.PostgreSQL.Connection (Connection, executeRawVoid)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (decodeRows)
import Database.Orville.PostgreSQL.Internal.SqlType (SqlType
                                                     -- numeric types
                                                    , integer
                                                    , serial
                                                    , bigInteger
                                                    , bigserial
                                                    , double

                                                    -- textual-ish types
                                                    , boolean
                                                    , unboundedText
                                                    , fixedText
                                                    , boundedText
                                                    , textSearchVector

                                                    -- date types
                                                    , date
                                                    , timestamp

                                                    -- type conversions
                                                    , nullableType
                                                    )
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

sqlTypeSpecs :: Pool Connection -> Spec
sqlTypeSpecs pool = describe "SqlType decoding tests" $ do
  integerSpecs pool
  bigIntegerSpecs pool
  serialSpecs pool
  bigSerialSpecs pool
  doubleSpecs pool
  boolSpecs pool
  unboundedTextSpecs pool
  fixedTextSpecs pool
  boundedTextSpecs pool
  textSearchVectorSpecs pool
  dateSpecs pool
  timestampSpecs pool
  nullableSpecs pool

integerSpecs :: Pool Connection -> Spec
integerSpecs pool = do
  it "Testing the decode of INTEGER with value 0" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "INTEGER"
        , rawSqlValue = Just $ B8.pack $ show (0 :: Int)
        , sqlType = integer
        , expectedValue = 0
        }

  it "Testing the decode of INTEGER with value 2147483647" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "INTEGER"
        , rawSqlValue = Just $ B8.pack $ show (2147483647 :: Int)
        , sqlType = integer
        , expectedValue = 2147483647
        }

  it "Testing the decode of INTEGER with value -2147483648" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "INTEGER"
        , rawSqlValue = Just $ B8.pack $ show (-2147483648 :: Int)
        , sqlType = integer
        , expectedValue = -2147483648
        }

bigIntegerSpecs :: Pool Connection -> Spec
bigIntegerSpecs pool = do
  it "Testing the decode of BIGINT with value 0" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BIGINT"
        , rawSqlValue = Just $ B8.pack $ show (0 :: Int64)
        , sqlType = bigInteger
        , expectedValue = 0
        }

  it "Testing the decode of BIGINT with value 21474836470" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BIGINT"
        , rawSqlValue = Just $ B8.pack $ show (21474836470 :: Int64)
        , sqlType = bigInteger
        , expectedValue = 21474836470
        }

serialSpecs :: Pool Connection -> Spec
serialSpecs pool = do
  it "Testing the decode of SERIAL with value 0" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "SERIAL"
        , rawSqlValue = Just $ B8.pack $ show (0 :: Int)
        , sqlType = serial
        , expectedValue = 0
        }

  it "Testing the decode of SERIAL with value 2147483647" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "SERIAL"
        , rawSqlValue = Just $ B8.pack $ show (2147483647 :: Int)
        , sqlType = serial
        , expectedValue = 2147483647
        }

  it "Testing the decode of SERIAL with value -2147483648" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "SERIAL"
        , rawSqlValue = Just $ B8.pack $ show (-2147483648 :: Int)
        , sqlType = serial
        , expectedValue = -2147483648
        }

bigSerialSpecs :: Pool Connection -> Spec
bigSerialSpecs pool = do
  it "Testing the decode of BIGSERIAL with value 0" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BIGSERIAL"
        , rawSqlValue = Just $ B8.pack $ show (0 :: Int64)
        , sqlType = bigserial
        , expectedValue = 0
        }

  it "Testing the decode of BIGSERIAL with value 21474836470" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BIGSERIAL"
        , rawSqlValue = Just $ B8.pack $ show (21474836470 :: Int64)
        , sqlType = bigserial
        , expectedValue = 21474836470
        }

doubleSpecs :: Pool Connection -> Spec
doubleSpecs pool = do
  it "Testing the decode of DOUBLE PRECISION with value 0" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "DOUBLE PRECISION"
        , rawSqlValue = Just $ B8.pack $ show (0.0 :: Double)
        , sqlType = double
        , expectedValue = 0.0
        }

  it "Testing the decode of DOUBLE PRECISION with value 1.5" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "DOUBLE PRECISION"
        , rawSqlValue = Just $ B8.pack $ show (1.5 :: Double)
        , sqlType = double
        , expectedValue = 1.5
        }

boolSpecs :: Pool Connection -> Spec
boolSpecs pool = do
  it "Testing the decode of BOOL with value False" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BOOL"
        , rawSqlValue = Just $ B8.pack $ show False
        , sqlType = boolean
        , expectedValue = False
        }

  it "Testing the decode of BOOL with value True" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "BOOL"
        , rawSqlValue = Just $ B8.pack $ show True
        , sqlType = boolean
        , expectedValue = True
        }

unboundedTextSpecs :: Pool Connection -> Spec
unboundedTextSpecs pool = do
  it "Testing the decode of TEXT with value abcde" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TEXT"
        , rawSqlValue = Just $ B8.pack "abcde"
        , sqlType = unboundedText
        , expectedValue = T.pack "abcde"
        }

fixedTextSpecs :: Pool Connection -> Spec
fixedTextSpecs pool = do
  it "Testing the decode of CHAR(5) with value 'abcde'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "CHAR(5)"
        , rawSqlValue = Just $ B8.pack "abcde"
        , sqlType = fixedText 5
        , expectedValue = T.pack "abcde"
        }

  it "Testing the decode of CHAR(5) with value 'fghi'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "CHAR(5)"
        , rawSqlValue = Just $ B8.pack "fghi"
        , sqlType = fixedText 5
        , expectedValue = T.pack "fghi "
        }

boundedTextSpecs :: Pool Connection -> Spec
boundedTextSpecs pool = do
  it "Testing the decode of VARCHAR(5) with value 'abcde'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "VARCHAR(5)"
        , rawSqlValue = Just $ B8.pack "abcde"
        , sqlType = boundedText 5
        , expectedValue = T.pack "abcde"
        }

  it "Testing the decode of VARCHAR(5) with value 'fghi'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "VARCHAR(5)"
        , rawSqlValue = Just $ B8.pack "fghi"
        , sqlType = boundedText 5
        , expectedValue = T.pack "fghi"
        }

textSearchVectorSpecs :: Pool Connection -> Spec
textSearchVectorSpecs pool = do
  it "Testing the decode of TSVECTOR with value 'abcde'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TSVECTOR"
        , rawSqlValue = Just $ B8.pack "'abcde'"
        , sqlType = textSearchVector
        , expectedValue = T.pack "'abcde'"
        }

  it "Testing the decode of TSVECTOR with value 'fghi'" $ do
    dropAndRecreateTable pool "mytsvector" "TSVECTOR"
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TSVECTOR"
        , rawSqlValue = Just $ B8.pack "'fghi'"
        , sqlType = textSearchVector
        , expectedValue = T.pack "'fghi'"
        }

dateSpecs :: Pool Connection -> Spec
dateSpecs pool = do
  it "Testing the decode of DATE with value 2020-12-21" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "DATE"
        , rawSqlValue = Just $ B8.pack "'2020-12-21'"
        , sqlType = date
        , expectedValue = Time.fromGregorian 2020 12 21
        }

timestampSpecs :: Pool Connection -> Spec
timestampSpecs pool = do
  it "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32-00'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
        , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32-00'"
        , sqlType = timestamp
        , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)
        }

  it "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32+00'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
        , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32+00'"
        , sqlType = timestamp
        , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)
        }

nullableSpecs :: Pool Connection -> Spec
nullableSpecs pool = do
  it "Testing the decode of TEXT NULL with value 'abcde'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TEXT NULL"
        , rawSqlValue = Just $ B8.pack "abcde"
        , sqlType = nullableType unboundedText
        , expectedValue = Just (T.pack "abcde")
        }

  it "Testing the decode of TEXT NULL with text value 'null'" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TEXT NULL"
        , rawSqlValue = Just $ B8.pack "null"
        , sqlType = nullableType unboundedText
        , expectedValue = Just (T.pack "null")
        }

  it "Testing the decode of TEXT NULL with value NULL" $ do
    runDecodingTest pool $
      DecodingTest
        { sqlTypeDDL = "TEXT NULL"
        , rawSqlValue = Nothing
        , sqlType = nullableType unboundedText
        , expectedValue = Nothing
        }

data DecodingTest a =
  DecodingTest
    { sqlTypeDDL :: String
    , rawSqlValue :: Maybe B8.ByteString
    , sqlType :: SqlType a
    , expectedValue :: a
    }

runDecodingTest :: (Show a, Eq a) => Pool Connection -> DecodingTest a -> IO ()
runDecodingTest pool test = do
  dropAndRecreateTable pool "decoding_test" (sqlTypeDDL test)

  let
    tableName = Expr.rawTableName "decoding_test"

  RawSql.executeVoid pool $
    Expr.insertExprToSql $
      Expr.insertExpr
        tableName
        (Expr.insertSqlValues [[SqlValue.fromRawBytesNullable (rawSqlValue test)]])

  maybeResult <-
    RawSql.execute pool $
      Expr.queryExprToSql $
        Expr.queryExpr Expr.selectStar (Expr.tableExpr tableName Nothing)

  case maybeResult of
    Nothing ->
      expectationFailure "select returned nothing"

    Just res -> do
      (maybeA:_) <- decodeRows res (sqlType test)
      shouldBe maybeA (Just (expectedValue test))

dropAndRecreateTable :: Pool Connection -> String -> String -> IO ()
dropAndRecreateTable pool tableName columnTypeDDL = do
  executeRawVoid pool (B8.pack $ "DROP TABLE IF EXISTS " <> tableName) []
  executeRawVoid pool (B8.pack $ "CREATE TABLE " <> tableName <> "(foo " <> columnTypeDDL <> ")") []

