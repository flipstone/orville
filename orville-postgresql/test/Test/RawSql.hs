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

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

rawSqlTests :: Orville.ConnectionPool -> Property.Group
rawSqlTests pool =
  Property.group
    "RawSql"
    [ prop_concatenatesSQLStrings
    , prop_tracksPlaceholders
    , prop_escapesStringLiteralsForExamples
    , prop_escapesIdentifiersForExamples
    , prop_escapesStringLiteralsForConnections pool
    , prop_escapesIdentifiersForConnections pool
    ]

prop_concatenatesSQLStrings :: Property.NamedProperty
prop_concatenatesSQLStrings =
  Property.singletonNamedProperty "Builds concatenated sql from strings" $ do
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

prop_tracksPlaceholders :: Property.NamedProperty
prop_tracksPlaceholders =
  Property.singletonNamedProperty "Tracks value placeholders in concatenated order" $ do
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

prop_escapesStringLiteralsForExamples :: Property.NamedProperty
prop_escapesStringLiteralsForExamples =
  Property.singletonNamedProperty "Escapes and quotes string literals for examples" $ do
    let
      rawSql =
        RawSql.stringLiteral (B8.pack "Hello W'orld")

      expectedBytes =
        B8.pack "'Hello W''orld'"

      actualBytes =
        RawSql.toExampleBytes rawSql

    actualBytes === expectedBytes

prop_escapesIdentifiersForExamples :: Property.NamedProperty
prop_escapesIdentifiersForExamples =
  Property.singletonNamedProperty "Escapes and quotes identifiers for examples" $ do
    let
      rawSql =
        RawSql.identifier (B8.pack "Hello W\"orld")

      expectedBytes =
        B8.pack "\"Hello W\"\"orld\""

      actualBytes =
        RawSql.toExampleBytes rawSql

    actualBytes === expectedBytes

prop_escapesStringLiteralsForConnections :: Property.NamedDBProperty
prop_escapesStringLiteralsForConnections =
  Property.singletonNamedDBProperty "Escapes and quotes string literals for connections" $ \pool -> do
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

prop_escapesIdentifiersForConnections :: Property.NamedDBProperty
prop_escapesIdentifiersForConnections =
  Property.singletonNamedDBProperty "Escapes and quotes identifiers for connections" $ \pool -> do
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
