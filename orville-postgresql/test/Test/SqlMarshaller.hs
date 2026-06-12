module Test.SqlMarshaller
  ( sqlMarshallerTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as B8
import qualified Data.Either as Either
import qualified Data.Int as Int
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Execution.ExecutionResult as Result
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualSqlRows)
import Test.Orphans ()
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

sqlMarshallerTests :: Tasty.TestTree
sqlMarshallerTests =
  Tasty.testGroup
    "SqlMarshaller"
    [ TastyHH.testProperty
        "Can read a pure Int via SqlMarshaller"
        property_returnPureValue
    , TastyHH.testProperty
        "Can combine SqlMarshallers with <*>"
        property_combineWithApplicative
    , TastyHH.testProperty
        "Read a single field from a result row using marshallField"
        property_marshellField_readSingleField
    , TastyHH.testProperty
        "marshallField fails gracefully when decoding a non-existent column"
        prop_marshallField_missingColumn
    , TastyHH.testProperty
        "marshallField fails gracefully when failing to decode a value"
        prop_marshallField_decodeValueFailure
    , TastyHH.testProperty
        "marshallResultFromSql decodes all rows in Foo result set"
        prop_marshallResultFromSql_Foo
    , TastyHH.testProperty
        "marshallResultFromSql decodes all rows in Foo result set, when given an alias for each field"
        prop_marshallResultFromSql_FooWithQualifier
    , TastyHH.testProperty
        "marshallResultFromSql decodes all rows in Bar result set"
        prop_marshallResultFromSql_Bar
    , TastyHH.testProperty
        "foldMarshallerFields collects all fields as their sql values"
        prop_foldMarshallerFields
    , TastyHH.testProperty
        "can pass a Maybe through SqlMarshaller"
        prop_passMaybeThrough
    , TastyHH.testProperty
        "can use marshallPartial to fail decoding with in a custom way"
        prop_partialMap
    , TastyHH.testProperty
        "toComparableSqlValue encodes the marshaller's write entity as a row value"
        prop_toComparableSqlValue
    , TastyHH.testProperty
        "referenceValueExpression returns a row value containing references to the marshaller's write fields"
        prop_referenceValueExpression
    ]

property_returnPureValue :: HH.Property
property_returnPureValue =
  HH.property $ do
    someInt <- HH.forAll generateInt
    result <- marshallTestRowFromSql (pure someInt) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [someInt]

property_combineWithApplicative :: HH.Property
property_combineWithApplicative =
  HH.property $ do
    firstInt <- HH.forAll generateInt
    secondInt <- HH.forAll generateInt
    result <- marshallTestRowFromSql ((pure (+ firstInt)) <*> (pure secondInt)) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [firstInt + secondInt]

property_marshellField_readSingleField :: HH.Property
property_marshellField_readSingleField =
  HH.property $ do
    targetName <- HH.forAll generateName
    targetValue <- HH.forAll generateInt32

    namesBefore <- HH.forAll (generateNamesOtherThan targetName)
    namesAfter <- HH.forAll (generateNamesOtherThan targetName)

    valuesBefore <- HH.forAll (generateAssociatedValues namesBefore generateInt32)
    valuesAfter <- HH.forAll (generateAssociatedValues namesAfter generateInt32)

    let
      fieldDef = Marshall.integerField targetName
      marshaller = Marshall.marshallField id fieldDef
      input =
        Result.mkFakeLibPQResult
          (map B8.pack (namesBefore ++ (targetName : namesAfter)))
          [map SqlValue.fromInt32 (valuesBefore ++ (targetValue : valuesAfter))]

    result <- marshallTestRowFromSql marshaller input
    Bifunctor.first show result === Right [targetValue]

prop_marshallField_missingColumn :: HH.Property
prop_marshallField_missingColumn =
  HH.property $ do
    targetName <- HH.forAll generateName
    otherNames <- HH.forAll (generateNamesOtherThan targetName)
    otherValues <- HH.forAll (generateAssociatedValues otherNames generateInt32)

    let
      fieldDef = Marshall.integerField targetName
      marshaller = Marshall.marshallField id fieldDef
      input =
        Result.mkFakeLibPQResult
          (map B8.pack otherNames)
          [map SqlValue.fromInt32 otherValues]

      expectedError =
        Marshall.MarshallError
          { Marshall.marshallErrorDetailLevel = ErrorDetailLevel.maximalErrorDetailLevel
          , Marshall.marshallErrorRowIdentifier = mempty
          , Marshall.marshallErrorDetails =
              Marshall.MissingColumnError $
                Marshall.MissingColumnErrorDetails
                  { Marshall.missingColumnName = B8.pack targetName
                  , Marshall.actualColumnNames = Set.fromList $ fmap B8.pack otherNames
                  }
          }

    result <- marshallTestRowFromSql marshaller input
    -- Use show on the error here so MarshallError and friends don't
    -- need an Eq instance
    Bifunctor.first show result === Left (show expectedError)

prop_marshallField_decodeValueFailure :: HH.Property
prop_marshallField_decodeValueFailure =
  HH.property $ do
    targetName <- HH.forAll generateName
    nonIntegerText <- HH.forAll (Gen.text (Range.linear 0 10) Gen.alpha)

    let
      fieldDef = Marshall.integerField targetName
      marshaller = Marshall.marshallField id fieldDef
      input =
        Result.mkFakeLibPQResult
          [B8.pack targetName]
          [[SqlValue.fromText nonIntegerText]]

    result <- marshallTestRowFromSql marshaller input

    case result of
      Right n -> do
        HH.annotateShow n
        HH.footnote "Expected decoding failure, but got success"
        HH.failure
      Left rowDecodeErr ->
        case Marshall.marshallErrorDetails rowDecodeErr of
          Marshall.DecodingError details ->
            map fst (Marshall.decodingErrorValues details) === [B8.pack targetName]
          err -> do
            HH.annotate $ Marshall.renderMarshallErrorDetails ErrorDetailLevel.maximalErrorDetailLevel err
            HH.footnote "Expected DecodingError error, but got another error instead."
            HH.failure

prop_marshallResultFromSql_Foo :: HH.Property
prop_marshallResultFromSql_Foo =
  HH.property $ do
    foos <- HH.forAll $ Gen.list (Range.linear 0 10) generateFoo

    let
      mkRowValues foo =
        [ SqlValue.fromText (fooName foo)
        , SqlValue.fromInt32 (fooSize foo)
        , maybe SqlValue.sqlNull SqlValue.fromBool (fooOption foo)
        ]

      input =
        Result.mkFakeLibPQResult
          [B8.pack "name", B8.pack "size", B8.pack "option"]
          (map mkRowValues foos)

    result <- marshallTestRowFromSql fooMarshaller input
    Bifunctor.first show result === Right foos

prop_marshallResultFromSql_FooWithQualifier :: HH.Property
prop_marshallResultFromSql_FooWithQualifier =
  HH.property $ do
    foos <- HH.forAll $ Gen.list (Range.linear 0 10) generateFoo

    let
      mkRowValues foo =
        [ SqlValue.fromText (fooName foo)
        , SqlValue.fromInt32 (fooSize foo)
        , maybe SqlValue.sqlNull SqlValue.fromBool (fooOption foo)
        ]

      input =
        Result.mkFakeLibPQResult
          [B8.pack "name", B8.pack "size", B8.pack "option"]
          (map mkRowValues foos)

    result <- marshallTestRowFromSql fooMarshallerWithQualifierOnEachField input
    Bifunctor.first show result === Right foos

prop_marshallResultFromSql_Bar :: HH.Property
prop_marshallResultFromSql_Bar =
  HH.property $ do
    bars <- HH.forAll $ Gen.list (Range.linear 0 10) generateBar

    let
      mkRowValues bar =
        [ SqlValue.fromDouble (barNumber bar)
        , maybe SqlValue.sqlNull SqlValue.fromText (barComment bar)
        , maybe SqlValue.sqlNull SqlValue.fromText (barLabel bar)
        ]

      input =
        Result.mkFakeLibPQResult
          [B8.pack "number", B8.pack "comment", B8.pack "label"]
          (map mkRowValues bars)

    result <- marshallTestRowFromSql barMarshaller input
    Bifunctor.first show result === Right bars

prop_foldMarshallerFields :: HH.Property
prop_foldMarshallerFields =
  HH.property $ do
    foo <- HH.forAll generateFoo

    let
      addField entry fields =
        case entry of
          Marshall.Natural _ fieldDef (Just getValue) ->
            (Marshall.fieldName fieldDef, Marshall.fieldValueToSqlValue fieldDef (getValue foo)) : fields
          Marshall.Natural _ _ Nothing ->
            fields
          Marshall.Synthetic _ ->
            fields

      actualFooRow =
        Marshall.foldMarshallerFields
          fooMarshaller
          []
          addField

      expectedFooRow =
        [ (Marshall.stringToFieldName "name", SqlValue.fromText $ fooName foo)
        , (Marshall.stringToFieldName "size", SqlValue.fromInt32 $ fooSize foo)
        , (Marshall.stringToFieldName "option", maybe SqlValue.sqlNull SqlValue.fromBool $ fooOption foo)
        ]

    [actualFooRow] `assertEqualSqlRows` [expectedFooRow]

prop_passMaybeThrough :: HH.Property
prop_passMaybeThrough =
  HH.property $ do
    someMaybeBool <- HH.forAll $ Gen.maybe Gen.bool
    result <- marshallTestRowFromSql (pure someMaybeBool) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [someMaybeBool]

prop_partialMap :: HH.Property
prop_partialMap =
  HH.property $ do
    texts <- HH.forAll $ Gen.list (Range.linear 0 10) (PgGen.pgText (Range.linear 0 10))

    let
      validateText text =
        if T.length text > 8
          then Left "Text too long"
          else Right text

      mkRowValues text =
        [ SqlValue.fromText text
        ]

      input =
        Result.mkFakeLibPQResult
          [B8.pack "text"]
          (map mkRowValues texts)

      marshaller =
        Marshall.marshallPartial $
          validateText
            <$> Marshall.marshallField id (Marshall.unboundedTextField "text")

      mkExpected text =
        case validateText text of
          Right validText ->
            Right validText
          Left message ->
            Left $
              -- Use show here to render the error so that MarshallError
              -- and friends don't need to have an Eq instance
              show $
                Marshall.MarshallError
                  { Marshall.marshallErrorDetailLevel = ErrorDetailLevel.maximalErrorDetailLevel
                  , Marshall.marshallErrorRowIdentifier = mempty
                  , Marshall.marshallErrorDetails =
                      Marshall.DecodingError $
                        Marshall.DecodingErrorDetails
                          { Marshall.decodingErrorValues = [(B8.pack "text", SqlValue.fromText text)]
                          , Marshall.decodingErrorMessage = message
                          }
                  }

      expected =
        traverse mkExpected texts

    HH.cover 1 (String.fromString "With no errors") (Either.isRight expected)
    HH.cover 1 (String.fromString "With at least one error") (Either.isLeft expected)

    result <- marshallTestRowFromSql marshaller input
    Bifunctor.first show result === expected

prop_toComparableSqlValue :: HH.Property
prop_toComparableSqlValue =
  HH.property $ do
    foo <- HH.forAll generateFoo

    let
      nameVal = SqlValue.fromText $ fooName foo
      sizeVal = SqlValue.fromInt32 $ fooSize foo
      optionVal = maybe SqlValue.sqlNull SqlValue.fromBool $ fooOption foo
      expected = SqlValue.fromRow $ NE.fromList [nameVal, sizeVal, optionVal]
      actual = Marshall.toComparableSqlValue fooMarshaller foo

    actual === expected

prop_referenceValueExpression :: HH.Property
prop_referenceValueExpression =
  Property.singletonProperty $ do
    let
      alias = Marshall.stringToAliasName "f"
      expected =
        Expr.rowValueConstructor $
          NE.fromList
            [ Expr.columnReference . Expr.untrackQualified . Marshall.qualifiedFieldColumnName $ Marshall.qualifyField alias nameField
            , Expr.columnReference . Expr.untrackQualified . Marshall.qualifiedFieldColumnName $ Marshall.qualifyField alias sizeField
            , Expr.columnReference . Expr.untrackQualified . Marshall.qualifiedFieldColumnName $ Marshall.qualifyField alias optionField
            ]
      actual = Marshall.referenceValueExpression (Marshall.marshallQualifyFields alias fooMarshaller)

    RawSql.toExampleBytes actual === RawSql.toExampleBytes expected

data Foo = Foo
  { fooName :: T.Text
  , fooSize :: Int.Int32
  , fooOption :: Maybe Bool
  }
  deriving (Eq, Show)

data Bar = Bar
  { barNumber :: Double
  , barComment :: Maybe T.Text
  , barLabel :: Maybe T.Text
  }
  deriving (Eq, Show)

fooMarshaller :: Marshall.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> Marshall.marshallField fooName nameField
    <*> Marshall.marshallField fooSize sizeField
    <*> Marshall.marshallField fooOption optionField

fooMarshallerWithQualifierOnEachField :: Marshall.SqlMarshaller Foo Foo
fooMarshallerWithQualifierOnEachField =
  Marshall.marshallQualifyFields (Marshall.stringToAliasName "f") $
    Foo
      <$> Marshall.marshallQualifyFields (Marshall.stringToAliasName "f") (Marshall.marshallField fooName nameField)
      <*> Marshall.marshallQualifyFields (Marshall.stringToAliasName "f") (Marshall.marshallField fooSize sizeField)
      <*> Marshall.marshallQualifyFields (Marshall.stringToAliasName "f") (Marshall.marshallField fooOption optionField)

nameField :: Marshall.FieldDefinition Marshall.NotNull T.Text
nameField =
  Marshall.unboundedTextField "name"

sizeField :: Marshall.FieldDefinition Marshall.NotNull Int.Int32
sizeField =
  Marshall.integerField "size"

optionField :: Marshall.FieldDefinition Marshall.Nullable (Maybe Bool)
optionField =
  Marshall.nullableField $ Marshall.booleanField "option"

generateFoo :: HH.Gen Foo
generateFoo =
  Foo
    <$> PgGen.pgText (Range.linear 0 16)
    <*> generateInt32
    <*> Gen.maybe Gen.bool

barMarshaller :: Marshall.SqlMarshaller Bar Bar
barMarshaller =
  Bar
    <$> Marshall.marshallField barNumber (Marshall.doubleField "number")
    <*> Marshall.marshallField barComment (Marshall.nullableField $ Marshall.unboundedTextField "comment")
    <*> Marshall.marshallField barLabel (Marshall.nullableField $ Marshall.boundedTextField "label" 16)

generateBar :: HH.Gen Bar
generateBar =
  Bar
    <$> PgGen.pgDouble
    <*> Gen.maybe (PgGen.pgText $ Range.linear 0 32)
    <*> Gen.maybe (PgGen.pgText $ Range.linear 1 16)

generateNamesOtherThan :: String -> HH.Gen [String]
generateNamesOtherThan specialName =
  Gen.list
    (Range.linear 0 10)
    (generateNameOtherThan specialName)

generateAssociatedValues :: [key] -> HH.Gen value -> HH.Gen [value]
generateAssociatedValues keys genValue =
  traverse (const genValue) keys

generateInt32 :: HH.Gen Int.Int32
generateInt32 =
  Gen.int32 (Range.exponentialFrom 0 minBound maxBound)

generateNameOtherThan :: String -> HH.Gen String
generateNameOtherThan specialName =
  Gen.filter (/= specialName) generateName

generateName :: HH.Gen String
generateName =
  Gen.string (Range.linear 1 256) Gen.alphaNum

generateInt :: HH.MonadGen m => m Int
generateInt = Gen.int $ Range.exponential 1 1024

marshallTestRowFromSql ::
  ( HH.MonadTest m
  , MIO.MonadIO m
  , Result.ExecutionResult result
  ) =>
  Marshall.SqlMarshaller writeEntity readEntity ->
  result ->
  m (Either Marshall.MarshallError [readEntity])
marshallTestRowFromSql marshaller input =
  HH.evalIO $
    Marshall.marshallResultFromSqlUsingRowIdExtractor
      ErrorDetailLevel.maximalErrorDetailLevel
      (Marshall.mkRowIdentityExtractor [] input)
      marshaller
      input
