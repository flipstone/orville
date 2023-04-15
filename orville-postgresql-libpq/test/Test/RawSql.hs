module Test.RawSql
  ( rawSqlTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Functor.Identity (runIdentity)
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

rawSqlTests :: Property.Group
rawSqlTests =
  Property.group
    "RawSql"
    [ prop_concatenatesSQLStrings
    , prop_tracksPlaceholders
    , prop_escapesStringLiteralsForExamples
    ]

prop_concatenatesSQLStrings :: Property.NamedProperty
prop_concatenatesSQLStrings =
  Property.singletonNamedProperty "Builds concatenated sql from strings" $ do
    let rawSql =
          RawSql.fromString "SELECT * "
            <> RawSql.fromString "FROM foo "
            <> RawSql.fromString "WHERE id = 1"

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = 1"

        (actualBytes, actualParams) =
          runIdentity $
            RawSql.toBytesAndParams RawSql.exampleEscaping rawSql

    actualBytes HH.=== expectedBytes
    actualParams HH.=== []

prop_tracksPlaceholders :: Property.NamedProperty
prop_tracksPlaceholders =
  Property.singletonNamedProperty "Tracks value placeholders in concatenated order" $ do
    let rawSql =
          RawSql.fromString "SELECT * "
            <> RawSql.fromString "FROM foo "
            <> RawSql.fromString "WHERE id = "
            <> RawSql.parameter (SqlValue.fromInt32 1)
            <> RawSql.fromString " AND "
            <> RawSql.fromString "bar IN ("
            <> RawSql.intercalate RawSql.comma bars
            <> RawSql.fromString ")"

        bars =
          map
            RawSql.parameter
            [ SqlValue.fromText (T.pack "pants")
            , SqlValue.fromText (T.pack "cheese")
            ]

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = $1 AND bar IN ($2,$3)"

        expectedParams =
          [ Just . PgTextFormatValue.fromByteString . B8.pack $ "1"
          , Just . PgTextFormatValue.fromByteString . B8.pack $ "pants"
          , Just . PgTextFormatValue.fromByteString . B8.pack $ "cheese"
          ]

        (actualBytes, actualParams) =
          runIdentity $
            RawSql.toBytesAndParams RawSql.exampleEscaping rawSql

    actualBytes HH.=== expectedBytes
    actualParams HH.=== expectedParams

prop_escapesStringLiteralsForExamples :: Property.NamedProperty
prop_escapesStringLiteralsForExamples =
  Property.singletonNamedProperty "Escapes and quotes string literals for examples" $ do
    let rawSql =
          RawSql.stringLiteral (B8.pack "Hel\\lo W'orld")

        expectedBytes =
          B8.pack "'Hel\\\\lo W\\'orld'"

        actualBytes =
          RawSql.toExampleBytes rawSql

    actualBytes HH.=== expectedBytes
