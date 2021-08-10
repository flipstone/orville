module Test.SqlType
  ( sqlTypeTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.ExecutionResult as ExecutionResult
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlType as SqlType
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

sqlTypeTests :: Pool.Pool Connection.Connection -> IO Bool
sqlTypeTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "SqlType decoding tests")
      $ integerTests pool
        <> bigIntegerTests pool
        <> serialTests pool
        <> bigSerialTests pool
        <> doubleTests pool
        <> boolTests pool
        <> unboundedTextTests pool
        <> fixedTextTests pool
        <> boundedTextTests pool
        <> textSearchVectorTests pool
        <> dateTests pool
        <> timestampTests pool
        <> nullableTests pool

integerTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
integerTests pool =
  [
    ( String.fromString "Testing the decode of INTEGER with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "INTEGER"
          , rawSqlValue = Just $ B8.pack $ show (0 :: Int)
          , sqlType = SqlType.integer
          , expectedValue = 0
          }
    )
  ,
    ( String.fromString "Testing the decode of INTEGER with value 2147483647"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "INTEGER"
          , rawSqlValue = Just $ B8.pack $ show (2147483647 :: Int)
          , sqlType = SqlType.integer
          , expectedValue = 2147483647
          }
    )
  ,
    ( String.fromString "Testing the decode of INTEGER with value -2147483648"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "INTEGER"
          , rawSqlValue = Just $ B8.pack $ show (-2147483648 :: Int)
          , sqlType = SqlType.integer
          , expectedValue = -2147483648
          }
    )
  ]

bigIntegerTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
bigIntegerTests pool =
  [
    ( String.fromString "Testing the decode of BIGINT with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BIGINT"
          , rawSqlValue = Just $ B8.pack $ show (0 :: Int.Int64)
          , sqlType = SqlType.bigInteger
          , expectedValue = 0
          }
    )
  ,
    ( String.fromString "Testing the decode of BIGINT with value 21474836470"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BIGINT"
          , rawSqlValue = Just $ B8.pack $ show (21474836470 :: Int.Int64)
          , sqlType = SqlType.bigInteger
          , expectedValue = 21474836470
          }
    )
  ]

serialTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
serialTests pool =
  [
    ( String.fromString "Testing the decode of SERIAL with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SERIAL"
          , rawSqlValue = Just $ B8.pack $ show (0 :: Int)
          , sqlType = SqlType.serial
          , expectedValue = 0
          }
    )
  ,
    ( String.fromString "Testing the decode of SERIAL with value 2147483647"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SERIAL"
          , rawSqlValue = Just $ B8.pack $ show (2147483647 :: Int)
          , sqlType = SqlType.serial
          , expectedValue = 2147483647
          }
    )
  ,
    ( String.fromString "Testing the decode of SERIAL with value -2147483648"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SERIAL"
          , rawSqlValue = Just $ B8.pack $ show (-2147483648 :: Int)
          , sqlType = SqlType.serial
          , expectedValue = -2147483648
          }
    )
  ]

bigSerialTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
bigSerialTests pool =
  [
    ( String.fromString "Testing the decode of BIGSERIAL with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BIGSERIAL"
          , rawSqlValue = Just $ B8.pack $ show (0 :: Int.Int64)
          , sqlType = SqlType.bigSerial
          , expectedValue = 0
          }
    )
  ,
    ( String.fromString "Testing the decode of BIGSERIAL with value 21474836470"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BIGSERIAL"
          , rawSqlValue = Just $ B8.pack $ show (21474836470 :: Int.Int64)
          , sqlType = SqlType.bigSerial
          , expectedValue = 21474836470
          }
    )
  ]

doubleTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
doubleTests pool =
  [
    ( String.fromString "Testing the decode of DOUBLE PRECISION with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "DOUBLE PRECISION"
          , rawSqlValue = Just $ B8.pack $ show (0.0 :: Double)
          , sqlType = SqlType.double
          , expectedValue = 0.0
          }
    )
  ,
    ( String.fromString "Testing the decode of DOUBLE PRECISION with value 1.5"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "DOUBLE PRECISION"
          , rawSqlValue = Just $ B8.pack $ show (1.5 :: Double)
          , sqlType = SqlType.double
          , expectedValue = 1.5
          }
    )
  ]

boolTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
boolTests pool =
  [
    ( String.fromString "Testing the decode of BOOL with value False"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BOOL"
          , rawSqlValue = Just $ B8.pack $ show False
          , sqlType = SqlType.boolean
          , expectedValue = False
          }
    )
  ,
    ( String.fromString "Testing the decode of BOOL with value True"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "BOOL"
          , rawSqlValue = Just $ B8.pack $ show True
          , sqlType = SqlType.boolean
          , expectedValue = True
          }
    )
  ]

unboundedTextTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
unboundedTextTests pool =
  [
    ( String.fromString "Testing the decode of TEXT with value abcde"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TEXT"
          , rawSqlValue = Just $ B8.pack "abcde"
          , sqlType = SqlType.unboundedText
          , expectedValue = T.pack "abcde"
          }
    )
  ]

fixedTextTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
fixedTextTests pool =
  [
    ( String.fromString "Testing the decode of CHAR(5) with value 'abcde'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "CHAR(5)"
          , rawSqlValue = Just $ B8.pack "abcde"
          , sqlType = SqlType.fixedText 5
          , expectedValue = T.pack "abcde"
          }
    )
  ,
    ( String.fromString "Testing the decode of CHAR(5) with value 'fghi'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "CHAR(5)"
          , rawSqlValue = Just $ B8.pack "fghi"
          , sqlType = SqlType.fixedText 5
          , expectedValue = T.pack "fghi "
          }
    )
  ]

boundedTextTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
boundedTextTests pool =
  [
    ( String.fromString "Testing the decode of VARCHAR(5) with value 'abcde'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "VARCHAR(5)"
          , rawSqlValue = Just $ B8.pack "abcde"
          , sqlType = SqlType.boundedText 5
          , expectedValue = T.pack "abcde"
          }
    )
  ,
    ( String.fromString "Testing the decode of VARCHAR(5) with value 'fghi'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "VARCHAR(5)"
          , rawSqlValue = Just $ B8.pack "fghi"
          , sqlType = SqlType.boundedText 5
          , expectedValue = T.pack "fghi"
          }
    )
  ]

textSearchVectorTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
textSearchVectorTests pool =
  [
    ( String.fromString "Testing the decode of TSVECTOR with value 'abcde'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TSVECTOR"
          , rawSqlValue = Just $ B8.pack "'abcde'"
          , sqlType = SqlType.textSearchVector
          , expectedValue = T.pack "'abcde'"
          }
    )
  ,
    ( String.fromString "Testing the decode of TSVECTOR with value 'fghi'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TSVECTOR"
          , rawSqlValue = Just $ B8.pack "'fghi'"
          , sqlType = SqlType.textSearchVector
          , expectedValue = T.pack "'fghi'"
          }
    )
  ]

dateTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
dateTests pool =
  [
    ( String.fromString "Testing the decode of DATE with value 2020-12-21"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "DATE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21'"
          , sqlType = SqlType.date
          , expectedValue = Time.fromGregorian 2020 12 21
          }
    )
  ]

timestampTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
timestampTests pool =
  [
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32-00'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32-00'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32+00'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32+00'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)
          }
    )
  ]

nullableTests :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
nullableTests pool =
  [
    ( String.fromString "Testing the decode of TEXT NULL with value 'abcde'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TEXT NULL"
          , rawSqlValue = Just $ B8.pack "abcde"
          , sqlType = SqlType.nullableType SqlType.unboundedText
          , expectedValue = Just (T.pack "abcde")
          }
    )
  ,
    ( String.fromString "Testing the decode of TEXT NULL with text value 'null'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TEXT NULL"
          , rawSqlValue = Just $ B8.pack "null"
          , sqlType = SqlType.nullableType SqlType.unboundedText
          , expectedValue = Just (T.pack "null")
          }
    )
  ,
    ( String.fromString "Testing the decode of TEXT NULL with value NULL"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TEXT NULL"
          , rawSqlValue = Nothing
          , sqlType = SqlType.nullableType SqlType.unboundedText
          , expectedValue = Nothing
          }
    )
  ]

data DecodingTest a = DecodingTest
  { sqlTypeDDL :: String
  , rawSqlValue :: Maybe B8.ByteString
  , sqlType :: SqlType.SqlType a
  , expectedValue :: a
  }

runDecodingTest :: (Show a, Eq a) => Pool.Pool Connection.Connection -> DecodingTest a -> HH.Property
runDecodingTest pool test =
  Property.singletonProperty $
    Pool.withResource pool $ \connection -> do
      MIO.liftIO $ dropAndRecreateTable connection "decoding_test" (sqlTypeDDL test)

      let tableName = Expr.rawTableName "decoding_test"

      MIO.liftIO . RawSql.executeVoid connection $
        Expr.insertExpr
          tableName
          Nothing
          (Expr.insertSqlValues [[SqlValue.fromRawBytesNullable (rawSqlValue test)]])

      result <-
        MIO.liftIO . RawSql.execute connection $
          Expr.queryExpr (Expr.selectClause $ Expr.selectExpr Nothing) Expr.selectStar (Expr.tableExpr tableName Nothing Nothing Nothing Nothing Nothing)

      (maybeA : _) <- MIO.liftIO $ ExecutionResult.decodeRows result (sqlType test)
      maybeA HH.=== Just (expectedValue test)

dropAndRecreateTable :: Connection.Connection -> String -> String -> IO ()
dropAndRecreateTable connection tableName columnTypeDDL = do
  Connection.executeRawVoid connection (B8.pack $ "DROP TABLE IF EXISTS " <> tableName) []
  Connection.executeRawVoid connection (B8.pack $ "CREATE TABLE " <> tableName <> "(foo " <> columnTypeDDL <> ")") []
