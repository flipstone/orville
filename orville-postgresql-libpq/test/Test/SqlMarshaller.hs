module Test.SqlMarshaller
  ( sqlMarshallerTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Int as Int
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Internal.ExecutionResult as Result
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition
import qualified Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.PGGen as PGGen

sqlMarshallerTests :: IO Bool
sqlMarshallerTests =
  HH.checkParallel $
    HH.Group
      (String.fromString "SqlMarshaller properties")
      [
        ( String.fromString "Can round read a pure Int via SqlMarshaller"
        , HH.property $ do
            someInt <- HH.forAll generateInt
            result <- MIO.liftIO $ SqlMarshaller.marshallRowFromSql (pure someInt) (Result.Row 0) (Result.mkFakeLibPQResult [] [])
            result HH.=== Right someInt
        )
      ,
        ( String.fromString "Can combine SqlMarshallers with <*>"
        , HH.property $ do
            firstInt <- HH.forAll generateInt
            secondInt <- HH.forAll generateInt
            result <- MIO.liftIO $ SqlMarshaller.marshallRowFromSql ((pure (+ firstInt)) <*> (pure secondInt)) (Result.Row 0) (Result.mkFakeLibPQResult [] [])
            result HH.=== Right (firstInt + secondInt)
        )
      ,
        ( String.fromString "Read a single field from a result row using marshallField"
        , HH.property $ do
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

            result <- MIO.liftIO $ SqlMarshaller.marshallRowFromSql marshaller (Result.Row 0) input
            result HH.=== Right targetValue
        )
      ,
        ( String.fromString "marshallField fails gracefully when decoding a non-existent column"
        , HH.property $ do
            targetName <- HH.forAll generateName
            otherNames <- HH.forAll (generateNamesOtherThan targetName)
            otherValues <- HH.forAll (generateAssociatedValues otherNames generateInt32)

            let fieldDef = FieldDefinition.integerField targetName
                marshaller = SqlMarshaller.marshallField id fieldDef
                input =
                  Result.mkFakeLibPQResult
                    (map B8.pack otherNames)
                    [map SqlValue.fromInt32 otherValues]

            result <- MIO.liftIO $ SqlMarshaller.marshallRowFromSql marshaller (Result.Row 0) input
            result HH.=== Left SqlMarshaller.FieldNotFoundInResultSet
        )
      ,
        ( String.fromString "marshallField fails gracefully when decoding a bad value"
        , HH.property $ do
            targetName <- HH.forAll generateName
            nonIntegerText <- HH.forAll (Gen.text (Range.linear 0 10) Gen.alpha)

            let fieldDef = FieldDefinition.integerField targetName
                marshaller = SqlMarshaller.marshallField id fieldDef
                input =
                  Result.mkFakeLibPQResult
                    [B8.pack targetName]
                    [[SqlValue.fromText nonIntegerText]]

            result <- MIO.liftIO $ SqlMarshaller.marshallRowFromSql marshaller (Result.Row 0) input
            result HH.=== Left SqlMarshaller.FailedToDecodeValue
        )
      ,
        ( String.fromString "marshallResultFromSql decodes all rows in result set"
        , HH.property $ do
            foos <- HH.forAll $ Gen.list (Range.linear 0 10) generateFoo

            let mkRowValues foo =
                  [SqlValue.fromText (fooName foo), SqlValue.fromInt32 (fooSize foo)]

                input =
                  Result.mkFakeLibPQResult
                    [B8.pack "name", B8.pack "size"]
                    (map mkRowValues foos)

            result <- MIO.liftIO $ SqlMarshaller.marshallResultFromSql fooMarshaller input
            result HH.=== Right foos
        )
      ,
        ( String.fromString "foldMarhallerFields collects all fields as their sql values"
        , HH.property $ do
            foo <- HH.forAll generateFoo

            let addField :: SqlMarshaller.FieldFold Foo [(FieldDefinition.FieldName, SqlValue.SqlValue)]
                addField fieldDef getValue fields =
                  (FieldDefinition.fieldName fieldDef, FieldDefinition.fieldValueToSqlValue fieldDef (getValue foo)) : fields

                actualFooRow =
                  SqlMarshaller.foldMarshallerFields
                    fooMarshaller
                    []
                    addField

                expectedFooRow =
                  [ (FieldDefinition.stringToFieldName "name", SqlValue.fromText $ fooName foo)
                  , (FieldDefinition.stringToFieldName "size", SqlValue.fromInt32 $ fooSize foo)
                  ]

            actualFooRow HH.=== expectedFooRow
        )
      ]

data Foo = Foo
  { fooName :: T.Text
  , fooSize :: Int.Int32
  }
  deriving (Eq, Show)

fooMarshaller :: SqlMarshaller.SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> SqlMarshaller.marshallField fooName (FieldDefinition.unboundedTextField "name")
    <*> SqlMarshaller.marshallField fooSize (FieldDefinition.integerField "size")

generateFoo :: HH.Gen Foo
generateFoo =
  Foo
    <$> PGGen.pgText (Range.linear 0 16)
    <*> generateInt32

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
