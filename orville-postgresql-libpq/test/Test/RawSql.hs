module Test.RawSql
  ( rawSqlSpecs
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

rawSqlSpecs :: Spec
rawSqlSpecs =
  describe "RawSql Tests" $ do
    it "Builds concatenated sql from strings" $ do
      let
        rawSql =
          RawSql.fromString "SELECT * "
          <> RawSql.fromString "FROM foo "
          <> RawSql.fromString "WHERE id = 1"

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = 1"

        (actualBytes, actualParams) =
          RawSql.toBytesAndParams rawSql

      actualBytes `shouldBe` expectedBytes
      actualParams `shouldBe` []

    it "Tracks value placeholders in concatenated order" $ do
      let
        rawSql =
          RawSql.fromString "SELECT * "
          <> RawSql.fromString "FROM foo "
          <> RawSql.fromString "WHERE id = "
          <> RawSql.parameter (SqlValue.fromInt32 1)
          <> RawSql.fromString " AND "
          <> RawSql.fromString "bar IN ("
          <> RawSql.intercalate (RawSql.fromString ",") bars
          <> RawSql.fromString ")"

        bars =
          map RawSql.parameter
            [ SqlValue.fromText (T.pack "pants")
            , SqlValue.fromText (T.pack "cheese")
            ]

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = $1 AND bar IN ($2,$3)"

        expectedParams =
          [ Just (B8.pack "1")
          , Just (B8.pack "pants")
          , Just (B8.pack "cheese")
          ]

        (actualBytes, actualParams) =
          RawSql.toBytesAndParams rawSql

      actualBytes `shouldBe` expectedBytes
      actualParams `shouldBe` expectedParams

