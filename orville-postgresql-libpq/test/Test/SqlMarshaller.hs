module Test.SqlMarshaller
  ( sqlMarshallerTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString.Char8 as B8
import qualified Data.Either as Either
import qualified Data.Int as Int
import qualified Data.Set as Set
import qualified Data.String as String
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Internal.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Internal.ExecutionResult as Result
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.MarshallError as MarshallError
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import Test.Expr.TestSchema (assertEqualSqlRows)
import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

sqlMarshallerTests :: Property.Group
sqlMarshallerTests =
  Property.group
    "SqlMarshaller"
    [ property_returnPureValue
    , property_combineWithApplicative
    , property_marshellField_readSingleField
    , prop_marshallField_missingColumn
    , prop_marshallField_decodeValueFailure
    , prop_marshallResultFromSql_Foo
    , prop_marshallResultFromSql_Bar
    , prop_foldMarshallerFields
    , prop_passMaybeThrough
    , prop_partialMap
    ]

property_returnPureValue :: Property.NamedProperty
property_returnPureValue =
  Property.namedProperty "Can read a pure Int via SqlMarshaller" $ do
    someInt <- HH.forAll generateInt
    result <- marshallTestRowFromSql (pure someInt) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [someInt]

property_combineWithApplicative :: Property.NamedProperty
property_combineWithApplicative =
  Property.namedProperty "Can combine SqlMarshallers with <*>" $ do
    firstInt <- HH.forAll generateInt
    secondInt <- HH.forAll generateInt
    result <- marshallTestRowFromSql ((pure (+ firstInt)) <*> (pure secondInt)) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [firstInt + secondInt]

property_marshellField_readSingleField :: Property.NamedProperty
property_marshellField_readSingleField =
  Property.namedProperty "Read a single field from a result row using marshallField" $ do
    targetName <- HH.forAll generateName
    targetValue <- HH.forAll generateInt32

    namesBefore <- HH.forAll (generateNamesOtherThan targetName)
    namesAfter <- HH.forAll (generateNamesOtherThan targetName)

    valuesBefore <- HH.forAll (generateAssociatedValues namesBefore generateInt32)
    valuesAfter <- HH.forAll (generateAssociatedValues namesAfter generateInt32)

    let fieldDef = FieldDefinition.integerField targetName
        marshaller = SqlMarshaller.marshallField id fieldDef
        input =
          Result.mkFakeLibPQResult
            (map B8.pack (namesBefore ++ (targetName : namesAfter)))
            [map SqlValue.fromInt32 (valuesBefore ++ (targetValue : valuesAfter))]

    result <- marshallTestRowFromSql marshaller input
    Bifunctor.first show result === Right [targetValue]

prop_marshallField_missingColumn :: Property.NamedProperty
prop_marshallField_missingColumn =
  Property.namedProperty "marshallField fails gracefully when decoding a non-existent column" $ do
    targetName <- HH.forAll generateName
    otherNames <- HH.forAll (generateNamesOtherThan targetName)
    otherValues <- HH.forAll (generateAssociatedValues otherNames generateInt32)

    let fieldDef = FieldDefinition.integerField targetName
        marshaller = SqlMarshaller.marshallField id fieldDef
        input =
          Result.mkFakeLibPQResult
            (map B8.pack otherNames)
            [map SqlValue.fromInt32 otherValues]

        expectedError =
          MarshallError.MarshallError
            { MarshallError.marshallErrorDetailLevel = ErrorDetailLevel.maximalErrorDetailLevel
            , MarshallError.marshallErrorRowIdentifier = mempty
            , MarshallError.marshallErrorDetails =
                MarshallError.MissingColumnError $
                  MarshallError.MissingColumnErrorDetails
                    { MarshallError.missingColumnName = B8.pack targetName
                    , MarshallError.actualColumnNames = Set.fromList $ fmap B8.pack otherNames
                    }
            }

    result <- marshallTestRowFromSql marshaller input
    -- Use show on the error here so MarshallError and friends don't
    -- need an Eq instance
    Bifunctor.first show result === Left (show expectedError)

prop_marshallField_decodeValueFailure :: Property.NamedProperty
prop_marshallField_decodeValueFailure =
  Property.namedProperty "marshallField fails gracefully when failing to decode a value" $ do
    targetName <- HH.forAll generateName
    nonIntegerText <- HH.forAll (Gen.text (Range.linear 0 10) Gen.alpha)

    let fieldDef = FieldDefinition.integerField targetName
        marshaller = SqlMarshaller.marshallField id fieldDef
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
        case MarshallError.marshallErrorDetails rowDecodeErr of
          MarshallError.DecodingError details ->
            map fst (MarshallError.decodingErrorValues details) === [B8.pack targetName]
          err -> do
            HH.annotate $ MarshallError.renderMarshallErrorDetails ErrorDetailLevel.maximalErrorDetailLevel err
            HH.footnote "Expected DecodingError error, but got another error instead."
            HH.failure

prop_marshallResultFromSql_Foo :: Property.NamedProperty
prop_marshallResultFromSql_Foo =
  Property.namedProperty "marshallResultFromSql decodes all rows in Foo result set" $ do
    foos <- HH.forAll $ Gen.list (Range.linear 0 10) generateFoo

    let mkRowValues foo =
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

prop_marshallResultFromSql_Bar :: Property.NamedProperty
prop_marshallResultFromSql_Bar =
  Property.namedProperty "marshallResultFromSql decodes all rows in Bar result set" $ do
    bars <- HH.forAll $ Gen.list (Range.linear 0 10) generateBar

    let mkRowValues bar =
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

prop_foldMarshallerFields :: Property.NamedProperty
prop_foldMarshallerFields =
  Property.namedProperty "foldMarshallerFields collects all fields as their sql values" $ do
    foo <- HH.forAll generateFoo

    let addField entry fields =
          case entry of
            SqlMarshaller.Natural fieldDef (Just getValue) ->
              (FieldDefinition.fieldName fieldDef, FieldDefinition.fieldValueToSqlValue fieldDef (getValue foo)) : fields
            SqlMarshaller.Natural _ Nothing ->
              fields
            SqlMarshaller.Synthetic _ ->
              fields

        actualFooRow =
          SqlMarshaller.foldMarshallerFields
            fooMarshaller
            []
            addField

        expectedFooRow =
          [ (FieldDefinition.stringToFieldName "name", SqlValue.fromText $ fooName foo)
          , (FieldDefinition.stringToFieldName "size", SqlValue.fromInt32 $ fooSize foo)
          , (FieldDefinition.stringToFieldName "option", maybe SqlValue.sqlNull SqlValue.fromBool $ fooOption foo)
          ]

    [actualFooRow] `assertEqualSqlRows` [expectedFooRow]

prop_passMaybeThrough :: Property.NamedProperty
prop_passMaybeThrough =
  Property.namedProperty "can pass a Maybe through SqlMarshaller" $ do
    someMaybeBool <- HH.forAll $ Gen.maybe Gen.bool
    result <- marshallTestRowFromSql (pure someMaybeBool) (Result.mkFakeLibPQResult [] [[]])
    Bifunctor.first show result === Right [someMaybeBool]

prop_partialMap :: Property.NamedProperty
prop_partialMap =
  Property.namedProperty "can use marshallPartial to fail decoding with in a custom way" $ do
    texts <- HH.forAll $ Gen.list (Range.linear 0 10) (PgGen.pgText (Range.linear 0 10))

    let validateText text =
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
          SqlMarshaller.marshallPartial $
            validateText
              <$> SqlMarshaller.marshallField id (FieldDefinition.unboundedTextField "text")

        mkExpected text =
          case validateText text of
            Right validText ->
              Right validText
            Left message ->
              Left $
                -- Use show here to render the error so that MarshallError
                -- and friends don't need to have an Eq instance
                show $
                  MarshallError.MarshallError
                    { MarshallError.marshallErrorDetailLevel = ErrorDetailLevel.maximalErrorDetailLevel
                    , MarshallError.marshallErrorRowIdentifier = mempty
                    , MarshallError.marshallErrorDetails =
                        MarshallError.DecodingError $
                          MarshallError.DecodingErrorDetails
                            { MarshallError.decodingErrorValues = [(B8.pack "text", SqlValue.fromText text)]
                            , MarshallError.decodingErrorMessage = message
                            }
                    }

        expected =
          traverse mkExpected texts

    HH.cover 1 (String.fromString "With no errors") (Either.isRight expected)
    HH.cover 1 (String.fromString "With at least one error") (Either.isLeft expected)

    result <- marshallTestRowFromSql marshaller input
    Bifunctor.first show result === expected

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

fooMarshaller :: SqlMarshaller.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> SqlMarshaller.marshallField fooName (FieldDefinition.unboundedTextField "name")
    <*> SqlMarshaller.marshallField fooSize (FieldDefinition.integerField "size")
    <*> SqlMarshaller.marshallField fooOption (FieldDefinition.nullableField $ FieldDefinition.booleanField "option")

generateFoo :: HH.Gen Foo
generateFoo =
  Foo
    <$> PgGen.pgText (Range.linear 0 16)
    <*> generateInt32
    <*> Gen.maybe Gen.bool

barMarshaller :: SqlMarshaller.SqlMarshaller Bar Bar
barMarshaller =
  Bar
    <$> SqlMarshaller.marshallField barNumber (FieldDefinition.doubleField "number")
    <*> SqlMarshaller.marshallField barComment (FieldDefinition.nullableField $ FieldDefinition.unboundedTextField "comment")
    <*> SqlMarshaller.marshallField barLabel (FieldDefinition.nullableField $ FieldDefinition.boundedTextField "label" 16)

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
  SqlMarshaller.SqlMarshaller writeEntity readEntity ->
  result ->
  m (Either MarshallError.MarshallError [readEntity])
marshallTestRowFromSql marshaller input =
  HH.evalIO $
    SqlMarshaller.marshallResultFromSqlUsingRowIdExtractor
      ErrorDetailLevel.maximalErrorDetailLevel
      (SqlMarshaller.mkRowIdentityExtractor [] input)
      marshaller
      input
