{-# LANGUAGE ScopedTypeVariables #-}

module Test.Connection
  ( connectionTests,
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Pool as Pool
import qualified Data.String as String
import qualified Data.Text.Encoding as Enc
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Connection as Connection
import qualified Orville.PostgreSQL.Internal.PGTextFormatValue as PGTextFormatValue

import qualified Test.PGGen as PGGen
import qualified Test.Property as Property

connectionTests :: Pool.Pool Connection.Connection -> Property.Group
connectionTests pool =
  Property.Group "Connection" $
    [
      ( String.fromString "executeRaw can pass non-null bytes equivalents whether checked for NUL or not"
      , HH.property $ do
          text <- HH.forAll $ PGGen.pgText (Range.linear 0 256)

          let notNulBytes =
                Enc.encodeUtf8 text

          value <-
            MIO.liftIO . Pool.withResource pool $ \connection -> do
              result <-
                Connection.executeRaw
                  connection
                  (B8.pack "SELECT $1::text = $2::text")
                  [ Just $ PGTextFormatValue.fromByteString notNulBytes
                  , Just $ PGTextFormatValue.unsafeFromByteString notNulBytes
                  ]

              LibPQ.getvalue' result 0 0

          value HH.=== Just (B8.pack "t")
      )
    ,
      ( String.fromString "executeRaw returns error if nul byte is given using safe constructor"
      , HH.property $ do
          textBefore <- HH.forAll $ PGGen.pgText (Range.linear 0 32)
          textAfter <- HH.forAll $ PGGen.pgText (Range.linear 0 32)

          let bytesWithNul =
                B8.concat
                  [ Enc.encodeUtf8 textBefore
                  , B8.pack "\NUL"
                  , Enc.encodeUtf8 textAfter
                  ]

          result <-
            MIO.liftIO . E.try . Pool.withResource pool $ \connection ->
              Connection.executeRaw
                connection
                (B8.pack "SELECT $1::text")
                [ Just $ PGTextFormatValue.fromByteString bytesWithNul
                ]

          case result of
            Left PGTextFormatValue.NULByteFoundError ->
              HH.success
            Right _ -> do
              HH.footnote "Expected 'executeRaw' to return failure, but it did not"
              HH.failure
      )
    ,
      ( String.fromString "executeRaw truncates values at the nul byte given using unsafe constructor"
      , HH.property $ do
          textBefore <- HH.forAll $ PGGen.pgText (Range.linear 0 32)
          textAfter <- HH.forAll $ PGGen.pgText (Range.linear 0 32)

          let bytesBefore =
                Enc.encodeUtf8 textBefore

              bytesWithNul =
                B8.concat
                  [ bytesBefore
                  , B8.pack "\NUL"
                  , Enc.encodeUtf8 textAfter
                  ]

          value <-
            MIO.liftIO . Pool.withResource pool $ \connection -> do
              result <-
                Connection.executeRaw
                  connection
                  (B8.pack "SELECT $1::text")
                  [ Just $ PGTextFormatValue.unsafeFromByteString bytesWithNul
                  ]

              LibPQ.getvalue' result 0 0

          value HH.=== Just bytesBefore
      )
    , -- Note: we only run this test once to cut down on the number of errors
      -- printed out by the database server when running tests repeatedly.

      ( String.fromString "executeRaw returns error if invalid sql is given"
      , Property.singletonProperty $ do
          -- We generate non-empty queries here becaues libpq returns different
          -- error details when an empty string is passed
          randomText <- HH.forAll $ PGGen.pgText (Range.constant 1 16)

          result <-
            MIO.liftIO . E.try . Pool.withResource pool $ \connection ->
              Connection.executeRaw
                connection
                (Enc.encodeUtf8 randomText)
                []

          case result of
            Left err -> do
              Connection.sqlExecutionErrorExecStatus err HH.=== LibPQ.FatalError

              let syntaxErrorState = B8.pack "42601"

              Connection.sqlExecutionErrorSqlState err HH.=== Just syntaxErrorState
            Right _ -> do
              HH.footnote "Expected 'executeRow' to return failure, but it did not"
              HH.failure
      )
    ]
