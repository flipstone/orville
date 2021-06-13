module Test.SqlMarshaller
  ( sqlMarshallerTree,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Int (Int32)
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Database.Orville.PostgreSQL.Internal.ExecutionResult (Row (..))
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as Result
import Database.Orville.PostgreSQL.Internal.FieldDefinition (integerField, stringToFieldName, unboundedTextField)
import Database.Orville.PostgreSQL.Internal.SqlMarshaller (MarshallError (..), SqlMarshaller, marshallEntityToSql, marshallField, marshallResultFromSql, marshallRowFromSql)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.PGGen as PGGen

sqlMarshallerTree :: TestTree
sqlMarshallerTree =
  testGroup
    "SqlMarshaller properties"
    [ testProperty "Can round read a pure Int via SqlMarshaller" . HH.property $ do
        someInt <- HH.forAll generateInt
        result <- liftIO $ marshallRowFromSql (pure someInt) (Row 0) (Result.mkFakeLibPQResult [] [])
        result === Right someInt
    , testProperty "Can combine SqlMarshallers with <*>" . HH.property $ do
        firstInt <- HH.forAll generateInt
        secondInt <- HH.forAll generateInt
        result <- liftIO $ marshallRowFromSql ((pure (+ firstInt)) <*> (pure secondInt)) (Row 0) (Result.mkFakeLibPQResult [] [])
        result === Right (firstInt + secondInt)
    , testProperty "Read a single field from a result row using marshallField" . HH.property $ do
        targetName <- HH.forAll generateName
        targetValue <- HH.forAll generateInt32

        namesBefore <- HH.forAll (generateNamesOtherThan targetName)
        namesAfter <- HH.forAll (generateNamesOtherThan targetName)

        valuesBefore <- HH.forAll (generateAssociatedValues namesBefore generateInt32)
        valuesAfter <- HH.forAll (generateAssociatedValues namesAfter generateInt32)

        let fieldDef = integerField targetName
            marshaller = marshallField id fieldDef
            input =
              Result.mkFakeLibPQResult
                (map B8.pack (namesBefore ++ (targetName : namesAfter)))
                [map SqlValue.fromInt32 (valuesBefore ++ (targetValue : valuesAfter))]

        result <- liftIO $ marshallRowFromSql marshaller (Row 0) input
        result === Right targetValue
    , testProperty "marshallField fails gracefully when decoding a non-existent column" . HH.property $ do
        targetName <- HH.forAll generateName
        otherNames <- HH.forAll (generateNamesOtherThan targetName)
        otherValues <- HH.forAll (generateAssociatedValues otherNames generateInt32)

        let fieldDef = integerField targetName
            marshaller = marshallField id fieldDef
            input =
              Result.mkFakeLibPQResult
                (map B8.pack otherNames)
                [map SqlValue.fromInt32 otherValues]

        result <- liftIO $ marshallRowFromSql marshaller (Row 0) input
        result === Left FieldNotFoundInResultSet
    , testProperty "marshallField fails gracefully when decoding a bad value" . HH.property $ do
        targetName <- HH.forAll generateName
        nonIntegerText <- HH.forAll (Gen.text (Range.linear 0 10) Gen.alpha)

        let fieldDef = integerField targetName
            marshaller = marshallField id fieldDef
            input =
              Result.mkFakeLibPQResult
                [B8.pack targetName]
                [[SqlValue.fromText nonIntegerText]]

        result <- liftIO $ marshallRowFromSql marshaller (Row 0) input
        result === Left FailedToDecodeValue
    , testProperty "marshallResultFromSql decodes all rows in result set" . HH.property $ do
        foos <- HH.forAll $ Gen.list (Range.linear 0 10) generateFoo

        let mkRowValues foo =
              [SqlValue.fromText (fooName foo), SqlValue.fromInt32 (fooSize foo)]

            input =
              Result.mkFakeLibPQResult
                [B8.pack "name", B8.pack "size"]
                (map mkRowValues foos)

        result <- liftIO $ marshallResultFromSql fooMarshaller input
        result === Right foos
    , testProperty "marshallEntityToSql collects all fields as their sql values" . HH.property $ do
        foo <- HH.forAll generateFoo

        let addField :: a -> b -> [(a, b)] -> [(a, b)]
            addField name sqlValue fields =
              (name, sqlValue) : fields

            actualFooRow =
              marshallEntityToSql
                fooMarshaller
                []
                addField
                foo

            expectedFooRow =
              [ (stringToFieldName "name", SqlValue.fromText $ fooName foo)
              , (stringToFieldName "size", SqlValue.fromInt32 $ fooSize foo)
              ]

        actualFooRow === expectedFooRow
    ]

data Foo = Foo
  { fooName :: T.Text
  , fooSize :: Int32
  }
  deriving (Eq, Show)

fooMarshaller :: SqlMarshaller Foo Foo
fooMarshaller =
  Foo
    <$> marshallField fooName (unboundedTextField "name")
    <*> marshallField fooSize (integerField "size")

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

generateInt32 :: HH.Gen Int32
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
