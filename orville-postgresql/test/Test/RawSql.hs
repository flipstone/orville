module Test.RawSql
  ( rawSqlTests
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity (runIdentity)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

rawSqlTests :: Orville.ConnectionPool -> Tasty.TestTree
rawSqlTests pool =
  Tasty.testGroup
    "RawSql"
    [ TastyHH.testProperty "Builds concatenated sql from strings" prop_concatenatesSQLStrings
    , TastyHH.testProperty "Tracks value placeholders in concatenated order" prop_tracksPlaceholders
    , TastyHH.testProperty "Escapes and quotes string literals for examples" prop_escapesStringLiteralsForExamples
    , TastyHH.testProperty "Escapes and quotes identifiers for examples" prop_escapesIdentifiersForExamples
    , TastyHH.testProperty "Escapes and quotes string literals for connections" (prop_escapesStringLiteralsForConnections pool)
    , TastyHH.testProperty "Escapes and quotes identifiers for connections" (prop_escapesIdentifiersForConnections pool)
    , TastyHH.testProperty "toParamCount is accurate" prop_toParamCountIsAccurate
    ]

prop_concatenatesSQLStrings :: HH.Property
prop_concatenatesSQLStrings =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.fromString "SELECT * "
          <> RawSql.fromString "FROM foo "
          <> RawSql.fromString "WHERE id = 1"

      expectedBytes =
        B8.pack "SELECT * FROM foo WHERE id = 1"

      (actualBytes, actualParams) =
        runIdentity $
          RawSql.toBytesAndParams RawSql.exampleQuoting rawSql

    actualBytes === expectedBytes
    actualParams === []

prop_tracksPlaceholders :: HH.Property
prop_tracksPlaceholders =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.fromString "SELECT * "
          <> RawSql.fromString "FROM foo "
          <> RawSql.fromString "WHERE id = "
          <> RawSql.parameter (SqlValue.fromInt32 1)
          <> RawSql.fromString " AND "
          <> RawSql.fromString "(bar, baz, id) IN ("
          <> RawSql.intercalate RawSql.comma bars
          <> RawSql.fromString ")"

      bars =
        map
          (RawSql.parameter . SqlValue.fromRow . NE.fromList)
          [ [SqlValue.fromText (T.pack "pants"), SqlValue.fromText (T.pack "ants"), SqlValue.fromInt32 2]
          , [SqlValue.fromText (T.pack "cheese"), SqlValue.fromText (T.pack "louise"), SqlValue.fromInt32 3]
          , [SqlValue.fromText (T.pack "lasagna"), SqlValue.fromText (T.pack "banana"), SqlValue.fromInt32 4]
          ]

      expectedBytes =
        B8.pack "SELECT * FROM foo WHERE id = $1 AND (bar, baz, id) IN (($2,$3,$4),($5,$6,$7),($8,$9,$10))"

      expectedParams =
        [ Just . PgTextFormatValue.fromByteString $ B8.pack "1"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "pants"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "ants"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "2"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "cheese"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "louise"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "3"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "lasagna"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "banana"
        , Just . PgTextFormatValue.fromByteString $ B8.pack "4"
        ]

      (actualBytes, actualParams) =
        runIdentity $
          RawSql.toBytesAndParams RawSql.exampleQuoting rawSql

    actualBytes === expectedBytes
    actualParams === expectedParams

prop_escapesStringLiteralsForExamples :: HH.Property
prop_escapesStringLiteralsForExamples =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.stringLiteral (B8.pack "Hello W'orld")

      expectedBytes =
        B8.pack "'Hello W''orld'"

      actualBytes =
        RawSql.toExampleBytes rawSql

    actualBytes === expectedBytes

prop_escapesIdentifiersForExamples :: HH.Property
prop_escapesIdentifiersForExamples =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.identifier (B8.pack "Hello W\"orld")

      expectedBytes =
        B8.pack "\"Hello W\"\"orld\""

      actualBytes =
        RawSql.toExampleBytes rawSql

    actualBytes === expectedBytes

prop_escapesStringLiteralsForConnections :: Orville.ConnectionPool -> HH.Property
prop_escapesStringLiteralsForConnections pool =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.stringLiteral (B8.pack "Hello W'orld")

      expectedBytes =
        B8.pack "'Hello W''orld'"

    (actualBytes, _) <-
      HH.evalIO $
        Conn.withPoolConnection pool $ \conn ->
          RawSql.toBytesAndParams (RawSql.connectionQuoting conn) rawSql

    actualBytes === expectedBytes

prop_escapesIdentifiersForConnections :: Orville.ConnectionPool -> HH.Property
prop_escapesIdentifiersForConnections pool =
  Property.singletonProperty $ do
    let
      rawSql =
        RawSql.identifier (B8.pack "Hello W\"orld")

      expectedBytes =
        B8.pack "\"Hello W\"\"orld\""

    (actualBytes, _) <-
      HH.evalIO $
        Conn.withPoolConnection pool $ \conn ->
          RawSql.toBytesAndParams (RawSql.connectionQuoting conn) rawSql

    actualBytes === expectedBytes

prop_toParamCountIsAccurate :: HH.Property
prop_toParamCountIsAccurate =
  HH.property $ do
    let
      genSqlAndParams =
        Gen.choice
          [ pure (RawSql.fromString "Foo", [0])
          , pure (RawSql.parameter (SqlValue.fromInt8 0), [1])
          , pure (RawSql.identifier (B8.pack "Bar"), [0])
          , pure (RawSql.stringLiteral (B8.pack "Baz"), [0])
          , pure (mempty, [0])
          , (<>) <$> Gen.small genSqlAndParams <*> Gen.small genSqlAndParams
          ]

      showSqlAndParams :: (RawSql.RawSql, [Int]) -> String
      showSqlAndParams (rawSql, params) =
        show (RawSql.toExampleBytes rawSql, params)

    (rawSql, params) <- HH.forAllWith showSqlAndParams genSqlAndParams

    RawSql.toParamCount rawSql === sum params
