module Test.FieldDefinition
  ( fieldDefinitionTests,
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.ExecutionResult as Result
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (sqlRowsToText)
import qualified Test.PGGen as PGGen
import qualified Test.Property as Property

fieldDefinitionTests :: Pool.Pool Connection.Connection -> IO Bool
fieldDefinitionTests pool =
  HH.checkSequential $
    HH.Group
      (String.fromString "FieldDefinition")
      $ integerField pool
        <> bigIntegerField pool
        <> doubleField pool
        <> booleanField pool
        <> unboundedTextField pool
        <> boundedTextField pool
        <> fixedTextField pool
        <> textSearchVectorField pool
        <> dateField pool
        <> timestampField pool

integerField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
integerField pool =
  testFieldProperties pool "integerField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.integerField "foo"
      , roundTripGen = PGGen.pgInt32
      }

bigIntegerField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
bigIntegerField pool =
  testFieldProperties pool "bigIntegerField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.bigIntegerField "foo"
      , roundTripGen = Gen.integral (Range.linearFrom 0 minBound maxBound)
      }

doubleField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
doubleField pool =
  testFieldProperties pool "doubleField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.doubleField "foo"
      , roundTripGen = PGGen.pgDouble
      }

booleanField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
booleanField pool =
  testFieldProperties pool "booleanField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.booleanField "foo"
      , roundTripGen = Gen.bool
      }

unboundedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
unboundedTextField pool =
  testFieldProperties pool "unboundedTextField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.unboundedTextField "foo"
      , roundTripGen = PGGen.pgText (Range.constant 0 1024)
      }

boundedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
boundedTextField pool =
  testFieldProperties pool "boundedTextField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.boundedTextField "foo" 4
      , roundTripGen = PGGen.pgText (Range.constant 0 4)
      }

fixedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
fixedTextField pool =
  testFieldProperties pool "fixedTextField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.fixedTextField "foo" 4
      , roundTripGen = PGGen.pgText (Range.constant 4 4)
      }

textSearchVectorField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
textSearchVectorField pool =
  testFieldProperties pool "textSearchVectorField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.textSearchVectorField "foo"
      , roundTripGen = tsVectorGen
      }

dateField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
dateField pool =
  testFieldProperties pool "dateField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.dateField "foo"
      , roundTripGen = dayGen
      }

timestampField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
timestampField pool =
  testFieldProperties pool "timestampField" $
    RoundTripTest
      { roundTripFieldDef = FieldDef.timestampField "foo"
      , roundTripGen = utcTimeGen
      }

testFieldProperties :: (Show a, Eq a) => Pool.Pool Connection.Connection -> String -> RoundTripTest a -> [(HH.PropertyName, HH.Property)]
testFieldProperties pool fieldDefName roundTripTest =
  [
    ( String.fromString (fieldDefName <> " - can round trip values (not null)")
    , HH.property $
        runRoundTripTest pool roundTripTest
    )
  ,
    ( String.fromString (fieldDefName <> " - can round trip values (nullable)")
    , HH.property $
        runNullableRoundTripTest pool roundTripTest
    )
  ,
    ( String.fromString (fieldDefName <> " - cannot insert null values into a not null field")
    , Property.singletonProperty $
        runNullCounterExampleTest pool roundTripTest
    )
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

runRoundTripTest :: (Show a, Eq a) => Pool.Pool Connection.Connection -> RoundTripTest a -> HH.PropertyT IO ()
runRoundTripTest pool testCase =
  Pool.withResource pool $ \connection -> do
    let fieldDef = roundTripFieldDef testCase

    value <- HH.forAll (roundTripGen testCase)

    rows <- MIO.liftIO $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
            (Expr.tableExpr testTable Nothing Nothing Nothing Nothing Nothing)

      Result.readRows result

    let roundTripResult =
          case rows of
            [[(_, sqlValue)]] ->
              Right (FieldDef.fieldValueFromSqlValue fieldDef sqlValue)
            _ ->
              Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

    roundTripResult HH.=== Right (Just value)

runNullableRoundTripTest :: (Show a, Eq a) => Pool.Pool Connection.Connection -> RoundTripTest a -> HH.PropertyT IO ()
runNullableRoundTripTest pool testCase =
  Pool.withResource pool $ \connection -> do
    let fieldDef = FieldDef.nullableField (roundTripFieldDef testCase)

    value <- HH.forAll (Gen.maybe $ roundTripGen testCase)

    HH.cover 1 (String.fromString "Nothing") (Maybe.isNothing value)
    HH.cover 20 (String.fromString "Just") (Maybe.isJust value)

    rows <- MIO.liftIO $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])

      result <-
        RawSql.execute connection $
          Expr.queryExpr
            (Expr.selectClause $ Expr.selectExpr Nothing)
            (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
            (Expr.tableExpr testTable Nothing Nothing Nothing Nothing Nothing)

      Result.readRows result

    let roundTripResult =
          case rows of
            [[(_, sqlValue)]] ->
              Right (FieldDef.fieldValueFromSqlValue fieldDef sqlValue)
            _ ->
              Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

    roundTripResult HH.=== Right (Just value)

runNullCounterExampleTest :: Pool.Pool Connection.Connection -> RoundTripTest a -> HH.PropertyT IO ()
runNullCounterExampleTest pool testCase =
  Pool.withResource pool $ \connection -> do
    let fieldDef = roundTripFieldDef testCase

    result <- MIO.liftIO . E.try $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[SqlValue.sqlNull]])

    case result of
      Left err ->
        Connection.sqlExecutionErrorSqlState err HH.=== Just (B8.pack "23502")
      Right _ -> do
        HH.footnote "Expected insert query to fail, but it did not"
        HH.failure

testTable :: Expr.TableName
testTable =
  Expr.rawTableName "field_definition_test"

dropAndRecreateTestTable :: FieldDef.FieldDefinition nullability a -> Connection.Connection -> IO ()
dropAndRecreateTestTable fieldDef connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)

  RawSql.executeVoid connection $
    Expr.createTableExpr testTable [FieldDef.fieldColumnDefinition fieldDef] Nothing
