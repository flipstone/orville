module Test.SqlMarshaller
  ( sqlMarshallerTree
  ) where

import           Data.Int (Int32)
import           Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty.Hedgehog (testProperty)
import           Test.Tasty (TestTree, testGroup)

import           Database.Orville.PostgreSQL.Internal.FieldDefinition (integerField)
import           Database.Orville.PostgreSQL.Internal.SqlMarshaller (marshallFromSql, mashallField, MarshallError(..))
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

sqlMarshallerTree :: TestTree
sqlMarshallerTree =
  testGroup "SqlMarshaller properties"
    [ testProperty "Can round read a pure Int via SqlMarshaller" . HH.property $ do
        someInt <- HH.forAll generateInt
        (marshallFromSql (pure someInt) []) === (Right someInt)

    , testProperty "Can combine SqlMarshallers with <*>" . HH.property $ do
        firstInt <- HH.forAll generateInt
        secondInt <- HH.forAll generateInt
        marshallFromSql ((pure (+ firstInt)) <*> (pure secondInt)) [] === (Right (firstInt + secondInt))

    , testProperty "Read a single field from a result row using mashallField" . HH.property $ do
        (targetName, targetValue) <- HH.forAll (generateNamedInt32 generateName)
        valuesBefore  <- HH.forAll (generateOtherFieldInt32s targetName)
        valuesAfter   <- HH.forAll (generateOtherFieldInt32s targetName)

        let
          fieldDef = integerField targetName
          marshaller = mashallField id fieldDef
          input =
            map
             (\(name, value) -> (name, SqlValue.fromInt32 value))
             (valuesBefore ++ (targetName, targetValue) : valuesAfter)

        marshallFromSql marshaller input === Right targetValue

    , testProperty "mashallField fails gracefully when decoding a non-existent column" . HH.property $ do
        targetName  <- HH.forAll generateName
        otherValues <- HH.forAll (generateOtherFieldInt32s targetName)

        let
          fieldDef = integerField targetName
          marshaller = mashallField id fieldDef
          input =
            map
             (\(name, value) -> (name, SqlValue.fromInt32 value))
             otherValues

        marshallFromSql marshaller input === Left FieldNotFoundInResultSet

    , testProperty "mashallField fails gracefully when decoding a bad value" . HH.property $ do
        targetName     <- HH.forAll generateName
        nonIntegerText <- HH.forAll (Gen.text (Range.linear 0 10) Gen.alpha)

        let
          fieldDef = integerField targetName
          marshaller = mashallField id fieldDef
          input = [(targetName, SqlValue.fromText nonIntegerText)]

        marshallFromSql marshaller input === Left FailedToDecodeValue
    ]

generateOtherFieldInt32s :: String -> HH.Gen [(String, Int32)]
generateOtherFieldInt32s specialName =
  Gen.list
    (Range.linear 0 10)
    (generateNamedInt32 (generateNameOtherThan specialName))

generateNamedInt32 :: HH.Gen String -> HH.Gen (String, Int32)
generateNamedInt32 genName =
  (,) <$> genName <*> Gen.int32 (Range.exponentialFrom 0 minBound maxBound)

generateNameOtherThan :: String -> HH.Gen String
generateNameOtherThan specialName =
  Gen.filter (/= specialName) generateName

generateName :: HH.Gen String
generateName =
  Gen.string (Range.linear 1 256) Gen.unicode

generateInt :: HH.MonadGen m => m Int
generateInt = Gen.int $ Range.exponential 1 1024
