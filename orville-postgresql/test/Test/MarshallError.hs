module Test.MarshallError
  ( marshallErrorTests
  )
where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as B8
import qualified Data.Set as Set
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Marshall.MarshallError as MarshallError
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

marshallErrorTests :: Orville.ConnectionPool -> Tasty.TestTree
marshallErrorTests pool =
  Tasty.testGroup
    "MarshallError"
    [ TastyHH.testProperty "Render decoding error with full details" prop_renderDecodingErrorWithFullDetails
    , TastyHH.testProperty "Render decoding error without schema names" prop_renderDecodingErrorWithoutSchemaNames
    , TastyHH.testProperty "Render decoding error without row id values" prop_renderDecodingErrorWithoutRowIdValues
    , TastyHH.testProperty "Render decoding error without non id values" prop_renderDecodingErrorWithoutNonIdValues
    , TastyHH.testProperty "Render decoding error without error message" prop_renderDecodingErrorWithoutErrorMessage
    , TastyHH.testProperty "Render missing column error with full details" prop_renderMissingErrorDetailsWithFullDetails
    , TastyHH.testProperty "Render missing column error without schema names" prop_renderMissingErrorDetailsWithoutSchemaNames
    , TastyHH.testProperty "Showing a MarshallError raised from an Orville context" (prop_showMarshallErrorRaisedFromOrvilleContext pool)
    ]

{- | Shared example used to test error detail level rendering of decoding errors below.

@since 1.0.0.0
-}
exampleDecodingError :: MarshallError.MarshallError
exampleDecodingError =
  MarshallError.MarshallError
    { MarshallError.marshallErrorDetailLevel = ErrorDetailLevel.minimalErrorDetailLevel
    , MarshallError.marshallErrorRowIdentifier = [(B8.pack "foo", SqlValue.fromInt8 1)]
    , MarshallError.marshallErrorDetails =
        MarshallError.DecodingError $
          MarshallError.DecodingErrorDetails
            { MarshallError.decodingErrorValues = [(B8.pack "bar", SqlValue.fromText (T.pack "baz"))]
            , MarshallError.decodingErrorMessage = "Failure"
            }
    }

prop_renderDecodingErrorWithFullDetails :: HH.Property
prop_renderDecodingErrorWithFullDetails =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ErrorDetailLevel.maximalErrorDetailLevel
          exampleDecodingError
    in
      rendered
        === "Unable to decode row with identifier [foo = 1]: \
            \Unable to decode columns from result set: Failure. \
            \Value(s) that failed to decode: [bar = baz]"

prop_renderDecodingErrorWithoutSchemaNames :: HH.Property
prop_renderDecodingErrorWithoutSchemaNames =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ( ErrorDetailLevel.maximalErrorDetailLevel
              { ErrorDetailLevel.includeSchemaNames = False
              }
          )
          exampleDecodingError
    in
      rendered
        === "Unable to decode row with identifier [[REDACTED] = 1]: \
            \Unable to decode columns from result set: Failure. \
            \Value(s) that failed to decode: [[REDACTED] = baz]"

prop_renderDecodingErrorWithoutRowIdValues :: HH.Property
prop_renderDecodingErrorWithoutRowIdValues =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ( ErrorDetailLevel.maximalErrorDetailLevel
              { ErrorDetailLevel.includeRowIdentifierValues = False
              }
          )
          exampleDecodingError
    in
      rendered
        === "Unable to decode row with identifier [foo = [REDACTED]]: \
            \Unable to decode columns from result set: Failure. \
            \Value(s) that failed to decode: [bar = baz]"

prop_renderDecodingErrorWithoutNonIdValues :: HH.Property
prop_renderDecodingErrorWithoutNonIdValues =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ( ErrorDetailLevel.maximalErrorDetailLevel
              { ErrorDetailLevel.includeNonIdentifierValues = False
              }
          )
          exampleDecodingError
    in
      rendered
        === "Unable to decode row with identifier [foo = 1]: \
            \Unable to decode columns from result set: Failure. \
            \Value(s) that failed to decode: [bar = [REDACTED]]"

prop_renderDecodingErrorWithoutErrorMessage :: HH.Property
prop_renderDecodingErrorWithoutErrorMessage =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ( ErrorDetailLevel.maximalErrorDetailLevel
              { ErrorDetailLevel.includeErrorMessage = False
              }
          )
          exampleDecodingError
    in
      rendered
        === "Unable to decode row with identifier [foo = 1]: \
            \Unable to decode columns from result set: [REDACTED]. \
            \Value(s) that failed to decode: [bar = baz]"

{- | Shared example used to test error detail level rendering of missing column errors below.

@since 1.0.0.0
-}
exampleMissingColumnError :: MarshallError.MarshallError
exampleMissingColumnError =
  MarshallError.MarshallError
    { MarshallError.marshallErrorDetailLevel = ErrorDetailLevel.minimalErrorDetailLevel
    , MarshallError.marshallErrorRowIdentifier = [(B8.pack "foo", SqlValue.fromInt8 1)]
    , MarshallError.marshallErrorDetails =
        MarshallError.MissingColumnError $
          MarshallError.MissingColumnErrorDetails
            { MarshallError.missingColumnName = B8.pack "baz"
            , MarshallError.actualColumnNames = Set.fromList [B8.pack "foo", B8.pack "bar"]
            }
    }

prop_renderMissingErrorDetailsWithFullDetails :: HH.Property
prop_renderMissingErrorDetailsWithFullDetails =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ErrorDetailLevel.maximalErrorDetailLevel
          exampleMissingColumnError
    in
      rendered
        === "Unable to decode row with identifier [foo = 1]: \
            \Column baz not found in results set. \
            \Actual columns were [bar, foo]"

prop_renderMissingErrorDetailsWithoutSchemaNames :: HH.Property
prop_renderMissingErrorDetailsWithoutSchemaNames =
  Property.singletonProperty $
    let
      rendered =
        MarshallError.renderMarshallError
          ( ErrorDetailLevel.maximalErrorDetailLevel
              { ErrorDetailLevel.includeSchemaNames = False
              }
          )
          exampleMissingColumnError
    in
      rendered
        === "Unable to decode row with identifier [[REDACTED] = 1]: \
            \Column [REDACTED] not found in results set. \
            \Actual columns were [[REDACTED], [REDACTED]]"

prop_showMarshallErrorRaisedFromOrvilleContext :: Orville.ConnectionPool -> HH.Property
prop_showMarshallErrorRaisedFromOrvilleContext pool =
  HH.property $ do
    errorDetailLevel <- HH.forAll generateErrorDetailLevel

    let
      sql =
        RawSql.fromString "SELECT 1 as id, 'notAnNumber' as number"

      marshaller =
        Orville.annotateSqlMarshaller [Orville.stringToFieldName "id"] $
          Orville.marshallField id (Orville.integerField "number")

      orvilleState =
        Orville.newOrvilleState errorDetailLevel pool

    result <-
      HH.evalIO $ do
        E.try . Orville.runOrvilleWithState orvilleState $ do
          Orville.executeAndDecode Orville.SelectQuery sql marshaller

    case result of
      Right _ -> do
        HH.annotate "Expected decoding result set to fail, but it did not"
        HH.failure
      Left marshallError ->
        show marshallError
          === MarshallError.renderMarshallError errorDetailLevel marshallError

generateErrorDetailLevel :: HH.Gen ErrorDetailLevel.ErrorDetailLevel
generateErrorDetailLevel =
  ErrorDetailLevel.ErrorDetailLevel
    <$> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
    <*> Gen.bool
