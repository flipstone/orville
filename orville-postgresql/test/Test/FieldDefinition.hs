module Test.FieldDefinition
  ( fieldDefinitionTests
  )
where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (sqlRowsToText)
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

fieldDefinitionTests :: Orville.ConnectionPool -> Property.Group
fieldDefinitionTests pool =
  Property.group "FieldDefinition" $
    integerField pool
      <> bigIntegerField pool
      <> doubleField pool
      <> booleanField pool
      <> unboundedTextField pool
      <> boundedTextField pool
      <> fixedTextField pool
      <> textSearchVectorField pool
      <> uuidField pool
      <> dateField pool
      <> utcTimestampField pool
      <> localTimestampField pool
      <> jsonbField pool

integerField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
integerField pool =
  testFieldProperties pool "integerField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.integerField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.integerDefault]
      , roundTripGen = PgGen.pgInt32
      }

bigIntegerField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
bigIntegerField pool =
  testFieldProperties pool "bigIntegerField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.bigIntegerField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.bigIntegerDefault]
      , roundTripGen = Gen.integral (Range.linearFrom 0 minBound maxBound)
      }

doubleField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
doubleField pool =
  testFieldProperties pool "doubleField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.doubleField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.doubleDefault]
      , roundTripGen = PgGen.pgDouble
      }

booleanField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
booleanField pool =
  testFieldProperties pool "booleanField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.booleanField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.booleanDefault]
      , roundTripGen = Gen.bool
      }

unboundedTextField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
unboundedTextField pool =
  testFieldProperties pool "unboundedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.unboundedTextField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 0 1024)
      }

boundedTextField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
boundedTextField pool =
  testFieldProperties pool "boundedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.boundedTextField "foo" 4
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 0 4)
      }

fixedTextField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
fixedTextField pool =
  testFieldProperties pool "fixedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.fixedTextField "foo" 4
      , roundTripDefaultValueTests = [RoundTripDefaultTest Marshall.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 4 4)
      }

textSearchVectorField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
textSearchVectorField pool =
  testFieldProperties pool "textSearchVectorField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.textSearchVectorField "foo"
      , roundTripDefaultValueTests = []
      , roundTripGen = tsVectorGen
      }

uuidField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
uuidField pool =
  testFieldProperties pool "uuidField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.uuidField "foo"
      , roundTripDefaultValueTests = []
      , roundTripGen = uuidGen
      }

dateField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
dateField pool =
  testFieldProperties pool "dateField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.dateField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest Marshall.dateDefault
          , InsertOnlyDefaultTest Marshall.currentDateDefault
          ]
      , roundTripGen = PgGen.pgDay
      }

utcTimestampField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
utcTimestampField pool =
  testFieldProperties pool "utcTimestampField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.utcTimestampField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest Marshall.utcTimestampDefault
          , InsertOnlyDefaultTest Marshall.currentUTCTimestampDefault
          ]
      , roundTripGen = PgGen.pgUTCTime
      }

localTimestampField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
localTimestampField pool =
  testFieldProperties pool "localTimestampField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.localTimestampField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest Marshall.localTimestampDefault
          , InsertOnlyDefaultTest Marshall.currentLocalTimestampDefault
          ]
      , roundTripGen = PgGen.pgLocalTime
      }

jsonbField :: Orville.ConnectionPool -> [(HH.PropertyName, HH.Property)]
jsonbField pool =
  testFieldProperties pool "jsonbField" $
    FieldDefinitionTest
      { roundTripFieldDef = Marshall.jsonbField "foo"
      , roundTripDefaultValueTests = []
      , roundTripGen = PgGen.pgJSON
      }

testFieldProperties ::
  (Show a, Eq a) =>
  Orville.ConnectionPool ->
  String ->
  FieldDefinitionTest a ->
  [(HH.PropertyName, HH.Property)]
testFieldProperties pool fieldDefName roundTripTest =
  ( ( String.fromString (fieldDefName <> " - can round trip values (not null)")
    , HH.property $ runRoundTripTest pool roundTripTest
    )
      : ( String.fromString (fieldDefName <> " - can round trip values (nullable)")
        , HH.property $ runNullableRoundTripTest pool roundTripTest
        )
      : ( String.fromString (fieldDefName <> " - cannot insert null values into a not null field")
        , Property.singletonProperty $ runNullCounterExampleTest pool roundTripTest
        )
      : map (testDefaultValueProperties pool fieldDefName roundTripTest) (roundTripDefaultValueTests roundTripTest)
  )

testDefaultValueProperties ::
  (Show a, Eq a) =>
  Orville.ConnectionPool ->
  String ->
  FieldDefinitionTest a ->
  DefaultValueTest a ->
  (HH.PropertyName, HH.Property)
testDefaultValueProperties pool fieldDefName roundTripTest defaultValueTest =
  case defaultValueTest of
    RoundTripDefaultTest mkDefaultValue ->
      ( String.fromString (fieldDefName <> " - can round trip a value inserted via a column default")
      , Property.singletonProperty $
          runDefaultValueFieldDefinitionTest pool roundTripTest mkDefaultValue
      )
    InsertOnlyDefaultTest defaultValue ->
      ( String.fromString (fieldDefName <> " - can insert an insert-only default value")
      , Property.singletonProperty $
          runDefaultValueInsertOnlyTest pool roundTripTest defaultValue
      )

-- This generator generates alphanumeric values currently because of syntax
-- issues with random characters being generated. There is a story to built
-- a better Haskell representation of TextSearchVector, which presumably will
-- help fix this.
tsVectorGen :: HH.Gen T.Text
tsVectorGen = do
  text <- Gen.text (Range.linear 1 1024) Gen.alphaNum
  pure $ T.concat [T.pack "'", text, T.pack "'"]

uuidGen :: HH.Gen UUID.UUID
uuidGen =
  UUID.fromWords
    <$> Gen.word32 Range.linearBounded
    <*> Gen.word32 Range.linearBounded
    <*> Gen.word32 Range.linearBounded
    <*> Gen.word32 Range.linearBounded

data FieldDefinitionTest a = FieldDefinitionTest
  { roundTripFieldDef :: Marshall.FieldDefinition Marshall.NotNull a
  , roundTripDefaultValueTests :: [DefaultValueTest a]
  , roundTripGen :: HH.Gen a
  }

data DefaultValueTest a
  = RoundTripDefaultTest (a -> Marshall.DefaultValue a)
  | InsertOnlyDefaultTest (Marshall.DefaultValue a)

runRoundTripTest :: (Show a, Eq a) => Orville.ConnectionPool -> FieldDefinitionTest a -> HH.PropertyT IO ()
runRoundTripTest pool testCase = do
  let
    fieldDef = roundTripFieldDef testCase

  value <- HH.forAll (roundTripGen testCase)
  rows <- HH.evalIO . Conn.withPoolConnection pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (Expr.insertSqlValues [[Marshall.toComparableSqlValue fieldDef value]])
        Nothing
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [Marshall.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr (Expr.tableFromItem testTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

    Execution.readRows result

  let
    roundTripResult =
      case rows of
        [[(_, sqlValue)]] ->
          Marshall.fieldValueFromSqlValue fieldDef sqlValue
        _ ->
          Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runNullableRoundTripTest :: (Show a, Eq a) => Orville.ConnectionPool -> FieldDefinitionTest a -> HH.PropertyT IO ()
runNullableRoundTripTest pool testCase = do
  let
    fieldDef = Marshall.nullableField (roundTripFieldDef testCase)

  value <-
    HH.forAll $
      Gen.frequency
        [ (1, pure Nothing)
        , (3, Just <$> roundTripGen testCase)
        ]

  HH.cover 1 (String.fromString "Nothing") (Maybe.isNothing value)
  HH.cover 20 (String.fromString "Just") (Maybe.isJust value)

  rows <- HH.evalIO . Conn.withPoolConnection pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (Expr.insertSqlValues [[Marshall.fieldValueToSqlValue fieldDef value]])
        Nothing
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [Marshall.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr (Expr.tableFromItem testTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

    Execution.readRows result

  let
    roundTripResult =
      case rows of
        [[(_, sqlValue)]] ->
          Marshall.fieldValueFromSqlValue fieldDef sqlValue
        _ ->
          Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runNullCounterExampleTest :: Orville.ConnectionPool -> FieldDefinitionTest a -> HH.PropertyT IO ()
runNullCounterExampleTest pool testCase = do
  result <- HH.evalIO . Conn.withPoolConnection pool $ \connection -> do
    let
      fieldDef = roundTripFieldDef testCase

    E.try $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[SqlValue.sqlNull]])
          Nothing
          Nothing

  case result of
    Left err ->
      Conn.sqlExecutionErrorSqlState err === Just (B8.pack "23502")
    Right _ -> do
      HH.footnote "Expected insert query to fail, but it did not"
      HH.failure

runDefaultValueFieldDefinitionTest ::
  (Show a, Eq a) =>
  Orville.ConnectionPool ->
  FieldDefinitionTest a ->
  (a -> Marshall.DefaultValue a) ->
  HH.PropertyT IO ()
runDefaultValueFieldDefinitionTest pool testCase mkDefaultValue = do
  value <- HH.forAll (roundTripGen testCase)

  let
    defaultValue =
      mkDefaultValue value

    fieldDef =
      Marshall.setDefaultValue defaultValue $ roundTripFieldDef testCase

  rows <- HH.evalIO . Conn.withPoolConnection pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (RawSql.unsafeSqlExpression "VALUES(DEFAULT)")
        Nothing
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [Marshall.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr (Expr.tableFromItem testTable) Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

    Execution.readRows result

  let
    roundTripResult =
      case rows of
        [[(_, sqlValue)]] ->
          Marshall.fieldValueFromSqlValue fieldDef sqlValue
        _ ->
          Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runDefaultValueInsertOnlyTest ::
  Orville.ConnectionPool ->
  FieldDefinitionTest a ->
  Marshall.DefaultValue a ->
  HH.PropertyT IO ()
runDefaultValueInsertOnlyTest pool testCase defaultValue =
  HH.evalIO . Conn.withPoolConnection pool $ \connection -> do
    let
      fieldDef =
        Marshall.setDefaultValue defaultValue $ roundTripFieldDef testCase

    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (RawSql.unsafeSqlExpression "VALUES(DEFAULT)")
        Nothing
        Nothing

testTable :: Expr.QualifiedOrUnqualified Expr.TableName
testTable =
  Expr.unqualified (Expr.tableName "field_definition_test")

dropAndRecreateTestTable :: Marshall.FieldDefinition nullability a -> Orville.Connection -> IO ()
dropAndRecreateTestTable fieldDef connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)

  RawSql.executeVoid connection $
    Expr.createTableExpr testTable [Marshall.fieldColumnDefinition fieldDef] Nothing []
