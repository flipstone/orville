module Test.RawSql
  ( rawSqlTests,
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.String as String
import qualified Data.Text as T
import qualified Hedgehog as HH

import qualified Database.Orville.PostgreSQL.Internal.PGTextFormatValue as PGTextFormatValue
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

rawSqlTests :: IO Bool
rawSqlTests =
  HH.checkParallel $
    HH.Group
      (String.fromString "RawSql Tests")
      [
        ( String.fromString "Builds concatenated sql from strings"
        , Property.singletonProperty $ do
            let rawSql =
                  RawSql.fromString "SELECT * "
                    <> RawSql.fromString "FROM foo "
                    <> RawSql.fromString "WHERE id = 1"

                expectedBytes =
                  B8.pack "SELECT * FROM foo WHERE id = 1"

                (actualBytes, actualParams) =
                  RawSql.toBytesAndParams rawSql

            actualBytes HH.=== expectedBytes
            actualParams HH.=== []
        )
      ,
        ( String.fromString "Tracks value placeholders in concatenated order"
        , Property.singletonProperty $ do
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
                  [ Just . PGTextFormatValue.fromByteString . B8.pack $ "1"
                  , Just . PGTextFormatValue.fromByteString . B8.pack $ "pants"
                  , Just . PGTextFormatValue.fromByteString . B8.pack $ "cheese"
                  ]

                (actualBytes, actualParams) =
                  RawSql.toBytesAndParams rawSql

            actualBytes HH.=== expectedBytes
            actualParams HH.=== expectedParams
        )
      ]
