module Test.SqlType
  ( sqlTypeTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Time as Time
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

sqlTypeTests :: Orville.ConnectionPool -> Property.Group
sqlTypeTests pool =
  Property.group "SqlType" $
    integerTests pool
      <> smallIntegerTests pool
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
      <> jsonbTests pool

integerTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

smallIntegerTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
smallIntegerTests pool =
  [
    ( String.fromString "Testing the decode of SMALLINT with value 0"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SMALLINT"
          , rawSqlValue = Just $ B8.pack $ show (0 :: Int)
          , sqlType = SqlType.smallInteger
          , expectedValue = 0
          }
    )
  ,
    ( String.fromString "Testing the decode of SMALLINT with value 32767"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SMALLINT"
          , rawSqlValue = Just $ B8.pack $ show (32767 :: Int)
          , sqlType = SqlType.smallInteger
          , expectedValue = 32767
          }
    )
  ,
    ( String.fromString "Testing the decode of SMALLINT with value -32768"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "SMALLINT"
          , rawSqlValue = Just $ B8.pack $ show (-32768 :: Int)
          , sqlType = SqlType.smallInteger
          , expectedValue = -32768
          }
    )
  ]

bigIntegerTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

serialTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

bigSerialTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

doubleTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

boolTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

unboundedTextTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

fixedTextTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

boundedTextTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

textSearchVectorTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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

jsonbTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
jsonbTests pool =
  [
    ( String.fromString "Testing the decode of graph '{\"key\": \"value\"}'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "JSONB"
          , rawSqlValue = Just $ B8.pack "{\"key\": \"value\"}"
          , sqlType = SqlType.jsonb
          , expectedValue = T.pack "{\"key\": \"value\"}"
          }
    )
  ]

dateTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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
  ,
    ( String.fromString "Testing the decode of DATE with value 0001-12-21"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "DATE"
          , rawSqlValue = Just $ B8.pack "'0001-12-21'"
          , sqlType = SqlType.date
          , expectedValue = Time.fromGregorian 1 12 21
          }
    )
  ,
    ( String.fromString "Testing the decode of DATE with value 10000-12-21"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "DATE"
          , rawSqlValue = Just $ B8.pack "'10000-12-21'"
          , sqlType = SqlType.date
          , expectedValue = Time.fromGregorian 10000 12 21
          }
    )
  ]

timestampTests :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
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
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-20 23:00:00-01'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-20 23:00:00-01'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) 0
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
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 01:00:00+01'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 01:00:00+01'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) 0
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:30:00+0030'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:30:00+0030'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) 0
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:30:00+00:30'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:30:00+00:30'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) 0
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITH TIME ZONE with value '2020-12-21 00:00:32.000+00'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITH TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32.000+00'"
          , sqlType = SqlType.timestamp
          , expectedValue = Time.UTCTime (Time.fromGregorian 2020 12 21) (Time.secondsToDiffTime 32)
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '2020-12-21 00:00:32'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 2020 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime 32)
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '2020-12-21 00:00:32.000'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:32.000'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 2020 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime 32)
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '2020-12-21 00:00:00.001'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:00:00.001'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 2020 12 21) (Time.timeToTimeOfDay $ Time.picosecondsToDiffTime (1 * 10 ^ (9 :: Int)))
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '2020-12-21 10:00:32'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 10:00:32'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 2020 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime (60 * 60 * 10 + 32))
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '2020-12-21 00:10:32'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'2020-12-21 00:10:32'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 2020 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime (60 * 10 + 32))
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '10000-12-21 00:00:32'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'10000-12-21 00:00:32'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 10000 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime 32)
          }
    )
  ,
    ( String.fromString "Testing the decode of TIMESTAMP WITHOUT TIME ZONE with value '0001-12-21 00:00:32'"
    , runDecodingTest pool $
        DecodingTest
          { sqlTypeDDL = "TIMESTAMP WITHOUT TIME ZONE"
          , rawSqlValue = Just $ B8.pack "'0001-12-21 00:00:32'"
          , sqlType = SqlType.timestampWithoutZone
          , expectedValue = Time.LocalTime (Time.fromGregorian 1 12 21) (Time.timeToTimeOfDay $ Time.secondsToDiffTime 32)
          }
    )
  ]

data DecodingTest a = DecodingTest
  { sqlTypeDDL :: String
  , rawSqlValue :: Maybe B8.ByteString
  , sqlType :: SqlType.SqlType a
  , expectedValue :: a
  }

runDecodingTest :: (Show a, Eq a) => Orville.ConnectionPool -> DecodingTest a -> HH.Property
runDecodingTest pool test =
  Property.singletonProperty $ do
    rows <- MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
      dropAndRecreateTable connection "decoding_test" (sqlTypeDDL test)

      let
        tableName = Expr.qualifyTable Nothing (Expr.tableName "decoding_test")

      RawSql.executeVoid connection $
        Expr.insertExpr
          tableName
          Nothing
          (Expr.insertSqlValues [[SqlValue.fromRawBytesNullable (rawSqlValue test)]])
          Nothing
          Nothing

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            Expr.selectStar
            (Just $ Expr.tableExpr (Expr.referencesTable tableName) Nothing Nothing Nothing Nothing Nothing Nothing)

      Execution.readRows result

    let
      actual = map (decodeSingleValue (sqlType test)) rows

    actual === [Right $ expectedValue test]

dropAndRecreateTable :: Orville.Connection -> String -> String -> IO ()
dropAndRecreateTable connection tableName columnTypeDDL = do
  RawSql.executeVoid connection (RawSql.fromString $ "DROP TABLE IF EXISTS " <> tableName)
  RawSql.executeVoid connection (RawSql.fromString $ "CREATE TABLE " <> tableName <> "(foo " <> columnTypeDDL <> ")")

decodeSingleValue :: SqlType.SqlType a -> [(key, SqlValue.SqlValue)] -> Either String a
decodeSingleValue valueType row =
  case row of
    [] ->
      Left "Unable to decode single value from empty row"
    (_, sqlValue) : _ ->
      SqlType.sqlTypeFromSql valueType sqlValue
