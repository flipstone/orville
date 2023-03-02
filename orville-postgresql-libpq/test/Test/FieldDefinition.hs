module Test.FieldDefinition
  ( fieldDefinitionTests,
  )
where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.DefaultValue as DefaultValue
import qualified Orville.PostgreSQL.Internal.ExecutionResult as Result
import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (sqlRowsToText)
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

fieldDefinitionTests :: Pool.Pool Connection.Connection -> Property.Group
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

integerField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
integerField pool =
  testFieldProperties pool "integerField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.integerField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.integerDefault]
      , roundTripGen = PgGen.pgInt32
      }

bigIntegerField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
bigIntegerField pool =
  testFieldProperties pool "bigIntegerField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.bigIntegerField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.bigIntegerDefault]
      , roundTripGen = Gen.integral (Range.linearFrom 0 minBound maxBound)
      }

doubleField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
doubleField pool =
  testFieldProperties pool "doubleField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.doubleField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.doubleDefault]
      , roundTripGen = PgGen.pgDouble
      }

booleanField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
booleanField pool =
  testFieldProperties pool "booleanField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.booleanField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.booleanDefault]
      , roundTripGen = Gen.bool
      }

unboundedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
unboundedTextField pool =
  testFieldProperties pool "unboundedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.unboundedTextField "foo"
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 0 1024)
      }

boundedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
boundedTextField pool =
  testFieldProperties pool "boundedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.boundedTextField "foo" 4
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 0 4)
      }

fixedTextField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
fixedTextField pool =
  testFieldProperties pool "fixedTextField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.fixedTextField "foo" 4
      , roundTripDefaultValueTests = [RoundTripDefaultTest DefaultValue.textDefault]
      , roundTripGen = PgGen.pgText (Range.constant 4 4)
      }

textSearchVectorField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
textSearchVectorField pool =
  testFieldProperties pool "textSearchVectorField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.textSearchVectorField "foo"
      , roundTripDefaultValueTests = []
      , roundTripGen = tsVectorGen
      }

uuidField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
uuidField pool =
  testFieldProperties pool "uuidField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.uuidField "foo"
      , roundTripDefaultValueTests = []
      , roundTripGen = uuidGen
      }

dateField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
dateField pool =
  testFieldProperties pool "dateField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.dateField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest DefaultValue.dateDefault
          , InsertOnlyDefaultTest DefaultValue.currentDateDefault
          ]
      , roundTripGen = PgGen.pgDay
      }

utcTimestampField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
utcTimestampField pool =
  testFieldProperties pool "utcTimestampField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.utcTimestampField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest DefaultValue.utcTimestampDefault
          , InsertOnlyDefaultTest DefaultValue.currentUTCTimestampDefault
          ]
      , roundTripGen = PgGen.pgUTCTime
      }

localTimestampField :: Pool.Pool Connection.Connection -> [(HH.PropertyName, HH.Property)]
localTimestampField pool =
  testFieldProperties pool "localTimestampField" $
    FieldDefinitionTest
      { roundTripFieldDef = FieldDef.localTimestampField "foo"
      , roundTripDefaultValueTests =
          [ RoundTripDefaultTest DefaultValue.localTimestampDefault
          , InsertOnlyDefaultTest DefaultValue.currentLocalTimestampDefault
          ]
      , roundTripGen = PgGen.pgLocalTime
      }

testFieldProperties ::
  (Show a, Eq a) =>
  Pool.Pool Connection.Connection ->
  String ->
  FieldDefinitionTest a ->
  [(HH.PropertyName, HH.Property)]
testFieldProperties pool fieldDefName roundTripTest =
  ( ( String.fromString (fieldDefName <> " - can round trip values (not null)")
    , HH.property $ runRoundTripTest pool roundTripTest
    ) :
    ( String.fromString (fieldDefName <> " - can round trip values (nullable)")
    , HH.property $ runNullableRoundTripTest pool roundTripTest
    ) :
    ( String.fromString (fieldDefName <> " - cannot insert null values into a not null field")
    , Property.singletonProperty $ runNullCounterExampleTest pool roundTripTest
    ) :
    map (testDefaultValueProperties pool fieldDefName roundTripTest) (roundTripDefaultValueTests roundTripTest)
  )

testDefaultValueProperties ::
  (Show a, Eq a) =>
  Pool.Pool Connection.Connection ->
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
  { roundTripFieldDef :: FieldDef.FieldDefinition FieldDef.NotNull a
  , roundTripDefaultValueTests :: [DefaultValueTest a]
  , roundTripGen :: HH.Gen a
  }

data DefaultValueTest a
  = RoundTripDefaultTest (a -> DefaultValue.DefaultValue a)
  | InsertOnlyDefaultTest (DefaultValue.DefaultValue a)

runRoundTripTest :: (Show a, Eq a) => Pool.Pool Connection.Connection -> FieldDefinitionTest a -> HH.PropertyT IO ()
runRoundTripTest pool testCase = do
  let fieldDef = roundTripFieldDef testCase

  value <- HH.forAll (roundTripGen testCase)
  rows <- HH.evalIO . Pool.withResource pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr testTable Nothing Nothing Nothing Nothing Nothing)

    Result.readRows result

  let roundTripResult =
        case rows of
          [[(_, sqlValue)]] ->
            FieldDef.fieldValueFromSqlValue fieldDef sqlValue
          _ ->
            Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runNullableRoundTripTest :: (Show a, Eq a) => Pool.Pool Connection.Connection -> FieldDefinitionTest a -> HH.PropertyT IO ()
runNullableRoundTripTest pool testCase = do
  let fieldDef = FieldDef.nullableField (roundTripFieldDef testCase)

  value <-
    HH.forAll $
      Gen.frequency
        [ (1, pure Nothing)
        , (3, Just <$> roundTripGen testCase)
        ]

  HH.cover 1 (String.fromString "Nothing") (Maybe.isNothing value)
  HH.cover 20 (String.fromString "Just") (Maybe.isJust value)

  rows <- HH.evalIO . Pool.withResource pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (Expr.insertSqlValues [[FieldDef.fieldValueToSqlValue fieldDef value]])
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr testTable Nothing Nothing Nothing Nothing Nothing)

    Result.readRows result

  let roundTripResult =
        case rows of
          [[(_, sqlValue)]] ->
            FieldDef.fieldValueFromSqlValue fieldDef sqlValue
          _ ->
            Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runNullCounterExampleTest :: Pool.Pool Connection.Connection -> FieldDefinitionTest a -> HH.PropertyT IO ()
runNullCounterExampleTest pool testCase = do
  result <- HH.evalIO . Pool.withResource pool $ \connection -> do
    let fieldDef = roundTripFieldDef testCase

    E.try $ do
      dropAndRecreateTestTable fieldDef connection

      RawSql.executeVoid connection $
        Expr.insertExpr
          testTable
          Nothing
          (Expr.insertSqlValues [[SqlValue.sqlNull]])
          Nothing

  case result of
    Left err ->
      Connection.sqlExecutionErrorSqlState err === Just (B8.pack "23502")
    Right _ -> do
      HH.footnote "Expected insert query to fail, but it did not"
      HH.failure

runDefaultValueFieldDefinitionTest ::
  (Show a, Eq a) =>
  Pool.Pool Connection.Connection ->
  FieldDefinitionTest a ->
  (a -> DefaultValue.DefaultValue a) ->
  HH.PropertyT IO ()
runDefaultValueFieldDefinitionTest pool testCase mkDefaultValue = do
  value <- HH.forAll (roundTripGen testCase)

  let defaultValue =
        mkDefaultValue value

      fieldDef =
        FieldDef.setDefaultValue defaultValue $ roundTripFieldDef testCase

  rows <- HH.evalIO . Pool.withResource pool $ \connection -> do
    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (RawSql.unsafeFromRawSql (RawSql.fromString "VALUES(DEFAULT)"))
        Nothing

    result <-
      RawSql.execute connection $
        Expr.queryExpr
          (Expr.selectClause $ Expr.selectExpr Nothing)
          (Expr.selectColumns [FieldDef.fieldColumnName fieldDef])
          (Just $ Expr.tableExpr testTable Nothing Nothing Nothing Nothing Nothing)

    Result.readRows result

  let roundTripResult =
        case rows of
          [[(_, sqlValue)]] ->
            FieldDef.fieldValueFromSqlValue fieldDef sqlValue
          _ ->
            Left ("Expected one row with one value in results, but got: " ++ show (sqlRowsToText rows))

  roundTripResult === Right value

runDefaultValueInsertOnlyTest ::
  Pool.Pool Connection.Connection ->
  FieldDefinitionTest a ->
  DefaultValue.DefaultValue a ->
  HH.PropertyT IO ()
runDefaultValueInsertOnlyTest pool testCase defaultValue =
  HH.evalIO . Pool.withResource pool $ \connection -> do
    let fieldDef =
          FieldDef.setDefaultValue defaultValue $ roundTripFieldDef testCase

    dropAndRecreateTestTable fieldDef connection

    RawSql.executeVoid connection $
      Expr.insertExpr
        testTable
        Nothing
        (RawSql.unsafeFromRawSql (RawSql.fromString "VALUES(DEFAULT)"))
        Nothing

testTable :: Expr.Qualified Expr.TableName
testTable =
  Expr.qualified Nothing (Expr.tableName "field_definition_test")

dropAndRecreateTestTable :: FieldDef.FieldDefinition nullability a -> Connection.Connection -> IO ()
dropAndRecreateTestTable fieldDef connection = do
  RawSql.executeVoid connection (RawSql.fromString "DROP TABLE IF EXISTS " <> RawSql.toRawSql testTable)

  RawSql.executeVoid connection $
    Expr.createTableExpr testTable [FieldDef.fieldColumnDefinition fieldDef] Nothing []
