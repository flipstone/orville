module Test.FieldDefinition
  ( fieldDefinitionTree,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Time as Time
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL.Connection (Connection)
import Database.Orville.PostgreSQL.Internal.ExecutionResult (readRows)
import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import qualified Database.Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

import qualified Test.PGGen as PGGen

fieldDefinitionTree :: Pool Connection -> TestTree
fieldDefinitionTree pool =
  testGroup
    "FieldDefinition"
    [ testProperty "can round trip an integer field" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.integerField "foo"
            , roundTripGen = PGGen.pgInt32
            }
    , testProperty "can round trip a bigIntegerField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.bigIntegerField "foo"
            , roundTripGen = Gen.integral (Range.linearFrom 0 minBound maxBound)
            }
    , testProperty "can round trip a doubleField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.doubleField "foo"
            , roundTripGen = PGGen.pgDouble
            }
    , testProperty "can round trip a booleanField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.booleanField "foo"
            , roundTripGen = Gen.bool
            }
    , testProperty "can round trip an unboundedTextField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.unboundedTextField "foo"
            , roundTripGen = PGGen.pgText (Range.constant 0 1024)
            }
    , testProperty "can round trip a boundedTextField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.boundedTextField "foo" 4
            , roundTripGen = PGGen.pgText (Range.constant 0 4)
            }
    , testProperty "can round trip a fixedTextField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.fixedTextField "foo" 4
            , roundTripGen = PGGen.pgText (Range.constant 4 4)
            }
    , testProperty "can round trip a textSearchVectorField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.textSearchVectorField "foo"
            , roundTripGen = tsVectorGen
            }
    , testProperty "can round trip a dateField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.dateField "foo"
            , roundTripGen = dayGen
            }
    , testProperty "can round trip a timestampField" . HH.property $ do
        runRoundTripTest pool $
          RoundTripTest
            { roundTripFieldDef = FieldDef.timestampField "foo"
            , roundTripGen = utcTimeGen
            }
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
runRoundTripTest pool testCase = do
  let fieldDef = roundTripFieldDef testCase

  value <- HH.forAll (roundTripGen testCase)

  rows <- liftIO $ do
    dropAndRecreateTestTable fieldDef pool

    RawSql.executeVoid pool $
      Expr.insertExprToSql $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])

    result <-
      RawSql.execute pool $
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

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "field_definition_test"

dropAndRecreateTestTable :: FieldDef.FieldDefinition FieldDef.NotNull a -> Pool Connection -> IO ()
dropAndRecreateTestTable fieldDef pool = do
  RawSql.executeVoid pool (RawSql.fromString "DROP TABLE IF EXISTS " <> Expr.tableNameToSql testTable)

  RawSql.executeVoid pool $
    Expr.createTableExprToSql $
      Expr.createTableExpr testTable [FieldDef.fieldColumnDefinition fieldDef] Nothing
