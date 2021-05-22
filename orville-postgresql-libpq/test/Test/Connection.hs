module Test.Connection
  ( connectionTree
  ) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import           Data.Pool (Pool)
import qualified Data.Text.Encoding as Enc
import qualified Database.PostgreSQL.LibPQ as LibPQ
import           Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import           Database.Orville.PostgreSQL.Connection (Connection)
import qualified Database.Orville.PostgreSQL.Connection as Connection
import qualified Database.Orville.PostgreSQL.Internal.PGTextFormatValue as PGTextFormatValue

import qualified Test.PGGen as PGGen

connectionTree :: Pool Connection -> TestTree
connectionTree pool =
  testGroup "Connection"
    [ testProperty "executeRaw can pass non-null bytes equivalents whether checked for NUL or not" . HH.property $ do
        text <- HH.forAll $ PGGen.pgText (Range.linear 0 256)

        let
          notNulBytes =
            Enc.encodeUtf8 text

        result <-
          liftIO $ do
            mbLibPQResult <-
              Connection.executeRaw
                pool
                (B8.pack "SELECT $1::text = $2::text")
                [ Just $ PGTextFormatValue.fromByteString notNulBytes
                , Just $ PGTextFormatValue.unsafeFromByteString notNulBytes
                ]

            traverse (\r -> LibPQ.getvalue' r 0 0) mbLibPQResult

        result === Just (Just (B8.pack "t"))

    , testProperty "executeRaw returns error if nul byte is given using safe constructor" . HH.property $ do
        textBefore <- HH.forAll $ PGGen.pgText (Range.linear 0 32)
        textAfter <- HH.forAll $ PGGen.pgText (Range.linear 0 32)

        let
          bytesWithNul =
            B8.concat
              [ Enc.encodeUtf8 textBefore
              , B8.pack "\NUL"
              , Enc.encodeUtf8 textAfter
              ]

        result <-
          liftIO $ do
            Connection.executeRaw
              pool
              (B8.pack "SELECT $1::text")
              [ Just $ PGTextFormatValue.fromByteString bytesWithNul
              ]

        result === Nothing

    , testProperty "executeRaw truncates values at the nul byte given using unsafe constructor" . HH.property $ do
        textBefore <- HH.forAll $ PGGen.pgText (Range.linear 0 32)
        textAfter <- HH.forAll $ PGGen.pgText (Range.linear 0 32)

        let
          bytesBefore =
            Enc.encodeUtf8 textBefore

          bytesWithNul =
            B8.concat
              [ bytesBefore
              , B8.pack "\NUL"
              , Enc.encodeUtf8 textAfter
              ]

        result <-
          liftIO $ do
            mbLibPQResult <-
              Connection.executeRaw
                pool
                (B8.pack "SELECT $1::text")
                [ Just $ PGTextFormatValue.unsafeFromByteString bytesWithNul
                ]

            traverse (\r -> LibPQ.getvalue' r 0 0) mbLibPQResult

        result === Just (Just bytesBefore)
    ]
