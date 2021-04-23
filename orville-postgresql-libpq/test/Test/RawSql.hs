module Test.RawSql
  ( rawSqlSpecs
  ) where

import qualified Data.ByteString.Char8 as B8

import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
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
          <> RawSql.parameter (B8.pack "1")
          <> RawSql.fromString " AND "
          <> RawSql.fromString "bar IN ("
          <> RawSql.intercalate (RawSql.fromString ",") bars
          <> RawSql.fromString ")"

        bars =
          map RawSql.parameter
            [ B8.pack "pants"
            , B8.pack "cheese"
            ]

        expectedBytes =
          B8.pack "SELECT * FROM foo WHERE id = $1 AND bar IN ($2,$3)"

        expectedParams =
          [ B8.pack "1"
          , B8.pack "pants"
          , B8.pack "cheese"
          ]

        (actualBytes, actualParams) =
          RawSql.toBytesAndParams rawSql

      actualBytes `shouldBe` expectedBytes
      actualParams `shouldBe` expectedParams

