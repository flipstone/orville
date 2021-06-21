module Test.FieldDefinition
  ( fieldDefinitionTree,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import Data.Pool (Pool, withResource)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Time as Time
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL.Connection (Connection, sqlExecutionErrorSqlState)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (readRows)
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.PGGen as PGGen

fieldDefinitionTree :: Pool Connection -> TestTree
fieldDefinitionTree pool =
  testGroup
    "FieldDefinition"
    [ testFieldProperties pool "integerField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.integerField "foo"
          , roundTripGen = PGGen.pgInt32
          }
    , testFieldProperties pool "bigIntegerField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.bigIntegerField "foo"
          , roundTripGen = Gen.integral (Range.linearFrom 0 minBound maxBound)
          }
    , testFieldProperties pool "doubleField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.doubleField "foo"
          , roundTripGen = PGGen.pgDouble
          }
    , testFieldProperties pool "booleanField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.booleanField "foo"
          , roundTripGen = Gen.bool
          }
    , testFieldProperties pool "unboundedTextField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.unboundedTextField "foo"
          , roundTripGen = PGGen.pgText (Range.constant 0 1024)
          }
    , testFieldProperties pool "boundedTextField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.boundedTextField "foo" 4
          , roundTripGen = PGGen.pgText (Range.constant 0 4)
          }
    , testFieldProperties pool "fixedTextField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.fixedTextField "foo" 4
          , roundTripGen = PGGen.pgText (Range.constant 4 4)
          }
    , testFieldProperties pool "textSearchVectorField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.textSearchVectorField "foo"
          , roundTripGen = tsVectorGen
          }
    , testFieldProperties pool "dateField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.dateField "foo"
          , roundTripGen = dayGen
          }
    , testFieldProperties pool "timestampField" $
        RoundTripTest
          { roundTripFieldDef = FieldDef.timestampField "foo"
          , roundTripGen = utcTimeGen
          }
    ]

testFieldProperties :: (Show a, Eq a) => Pool Connection -> String -> RoundTripTest a -> TestTree
testFieldProperties pool fieldDefName roundTripTest =
  testGroup
    fieldDefName
    [ testProperty "can round trip values (not null)" . HH.property $ do
        runRoundTripTest pool roundTripTest
    , testProperty "can round trip values (nullable)" . HH.property $ do
        runNullableRoundTripTest pool roundTripTest
    , testProperty "cannot insert null values into a not null field" . HH.withTests 1 . HH.property $ do
        runNullCounterExampleTest pool roundTripTest
    ]

-- This generator generates alphanumeric values currently because of syntax
-- issues with random characters being generated. There is a story to built
-- a better Haskell representation of TextSearchVector, which presumably will
-- help fix this.
tsVectorGen :: HH.Gen T.Text
tsVectorGen = do
  text <- Gen.text (Range.linear 1 1024) Gen.alphaNum
  pure $ T.concat [T.pack "'", text, T.pack "'"]

utcTimeGen :: HH.Gen Time.UTCTime
utcTimeGen =
  Time.UTCTime <$> dayGen <*> timeOfDayGen

dayGen :: HH.Gen Time.Day
dayGen = do
  year <- Gen.integral (Range.linearFrom 2000 0 3000)
  month <- Gen.integral (Range.constant 1 12)
  day <- Gen.integral (Range.constant 1 (Time.gregorianMonthLength year month))

  pure (Time.fromGregorian year month day)

timeOfDayGen :: HH.Gen Time.DiffTime
timeOfDayGen =
  Time.secondsToDiffTime <$> Gen.integral (Range.constant 0 85399)

data RoundTripTest a = RoundTripTest
  { roundTripFieldDef :: FieldDef.FieldDefinition FieldDef.NotNull a
  , roundTripGen :: HH.Gen a
  }

runRoundTripTest :: (Show a, Eq a) => Pool Connection -> RoundTripTest a -> HH.PropertyT IO ()
runRoundTripTest pool testCase =
  withResource pool $ \connection -> do
    let fieldDef = roundTripFieldDef testCase

    value <- HH.forAll (roundTripGen testCase)

    rows <- liftIO $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExprToSql $
          Expr.insertExpr
            testTable
            Nothing
            (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])

      result <-
        RawSql.execute connection $
          Expr.queryExprToSql $
            Expr.queryExpr
              (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
              (Expr.tableExpr testTable Nothing Nothing)

      readRows result

    let roundTripResult =
          case rows of
            [[(_, sqlValue)]] ->
              Right (FieldDef.fieldValueFromSqlValue fieldDef sqlValue)
            _ ->
              Left ("Expected one row with one value in results, but got: " ++ show rows)

    roundTripResult === Right (Just value)

runNullableRoundTripTest :: (Show a, Eq a) => Pool Connection -> RoundTripTest a -> HH.PropertyT IO ()
runNullableRoundTripTest pool testCase =
  withResource pool $ \connection -> do
    let fieldDef = FieldDef.nullableField (roundTripFieldDef testCase)

    value <- HH.forAll (Gen.maybe $ roundTripGen testCase)

    HH.cover 1 (fromString "Nothing") (Maybe.isNothing value)
    HH.cover 20 (fromString "Just") (Maybe.isJust value)

    rows <- liftIO $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExprToSql $
          Expr.insertExpr
            testTable
            Nothing
            (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])

      result <-
        RawSql.execute connection $
          Expr.queryExprToSql $
            Expr.queryExpr
              (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
              (Expr.tableExpr testTable Nothing Nothing)

      readRows result

    let roundTripResult =
          case rows of
            [[(_, sqlValue)]] ->
              Right (FieldDef.fieldValueFromSqlValue fieldDef sqlValue)
            _ ->
              Left ("Expected one row with one value in results, but got: " ++ show rows)

    roundTripResult === Right (Just value)

runNullCounterExampleTest :: Pool Connection -> RoundTripTest a -> HH.PropertyT IO ()
runNullCounterExampleTest pool testCase =
  withResource pool $ \connection -> do
    let fieldDef = roundTripFieldDef testCase

    result <- liftIO . try $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExprToSql $
          Expr.insertExpr
            testTable
            Nothing
            (Expr.insertSqlValues [[SqlValue.sqlNull]])

    case result of
      Left err ->
        sqlExecutionErrorSqlState err === Just (B8.pack "23502")
      Right _ -> do
        HH.footnote "Expected insert query to fail, but it did not"
        HH.failure

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "field_definition_test"

dropAndRecreateTestTable :: FieldDef.FieldDefinition nullability a -> Connection -> IO ()
dropAndRecreateTestTable fieldDef connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)

  RawSql.executeVoid connection $
    Expr.createTableExprToSql $
      Expr.createTableExpr testTable [FieldDef.fieldColumnDefinition fieldDef] Nothing
