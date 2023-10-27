{-# LANGUAGE ScopedTypeVariables #-}

module Test.Connection
  ( connectionTests
  )
where

import qualified Control.Exception as E
import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as Enc
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue

import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

connectionTests :: Conn.ConnectionPool -> Property.Group
connectionTests pool =
  Property.Group "Connection" $
    [ prop_safeOrUnsafeNonNullBytes pool
    , prop_errorOnSafeNulByte pool
    , prop_truncateValuesAtUnsafeNulByte pool
    , prop_errorOnInvalidSql pool
    ]

prop_safeOrUnsafeNonNullBytes :: Property.NamedDBProperty
prop_safeOrUnsafeNonNullBytes =
  Property.namedDBProperty "executeRaw can pass non-null bytes equivalents whether checked for NUL or not" $ \pool -> do
    text <- HH.forAll $ PgGen.pgText (Range.linear 0 256)

    let
      notNulBytes =
        Enc.encodeUtf8 text

    value <-
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        result <-
          Conn.executeRaw
            connection
            (B8.pack "SELECT $1::text = $2::text")
            [ Just $ PgTextFormatValue.fromByteString notNulBytes
            , Just $ PgTextFormatValue.unsafeFromByteString notNulBytes
            ]

        LibPQ.getvalue' result 0 0

    value === Just (B8.pack "t")

prop_errorOnSafeNulByte :: Property.NamedDBProperty
prop_errorOnSafeNulByte =
  Property.namedDBProperty "executeRaw returns error if nul byte is given using safe constructor" $ \pool -> do
    textBefore <- HH.forAll $ PgGen.pgText (Range.linear 0 32)
    textAfter <- HH.forAll $ PgGen.pgText (Range.linear 0 32)

    let
      bytesWithNul =
        B8.concat
          [ Enc.encodeUtf8 textBefore
          , B8.pack "\NUL"
          , Enc.encodeUtf8 textAfter
          ]

    result <-
      MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection ->
        Conn.executeRaw
          connection
          (B8.pack "SELECT $1::text")
          [ Just $ PgTextFormatValue.fromByteString bytesWithNul
          ]

    case result of
      Left PgTextFormatValue.NULByteFoundError ->
        HH.success
      Right _ -> do
        HH.footnote "Expected 'executeRaw' to return failure, but it did not"
        HH.failure

prop_truncateValuesAtUnsafeNulByte :: Property.NamedDBProperty
prop_truncateValuesAtUnsafeNulByte =
  Property.namedDBProperty "executeRaw truncates values at the nul byte given using unsafe constructor" $ \pool -> do
    textBefore <- HH.forAll $ PgGen.pgText (Range.linear 0 32)
    textAfter <- HH.forAll $ PgGen.pgText (Range.linear 0 32)

    let
      bytesBefore =
        Enc.encodeUtf8 textBefore

      bytesWithNul =
        B8.concat
          [ bytesBefore
          , B8.pack "\NUL"
          , Enc.encodeUtf8 textAfter
          ]

    value <-
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        result <-
          Conn.executeRaw
            connection
            (B8.pack "SELECT $1::text")
            [ Just $ PgTextFormatValue.unsafeFromByteString bytesWithNul
            ]

        LibPQ.getvalue' result 0 0

    value === Just bytesBefore

prop_errorOnInvalidSql :: Property.NamedDBProperty
prop_errorOnInvalidSql =
  -- Note: we only run this test once to cut down on the number of errors
  -- printed out by the database server when running tests repeatedly.
  Property.singletonNamedDBProperty "executeRaw returns error if invalid sql is given" $ \pool -> do
    -- We generate non-empty queries here becaues libpq returns different
    -- error details when an empty string is passed
    randomText <- HH.forAll $ PgGen.pgText (Range.constant 1 16)

    result <-
      MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection ->
        Conn.executeRaw
          connection
          (Enc.encodeUtf8 randomText)
          []

    case result of
      Left err -> do
        Conn.sqlExecutionErrorExecStatus err === Just LibPQ.FatalError

        let
          syntaxErrorState = B8.pack "42601"

        Conn.sqlExecutionErrorSqlState err === Just syntaxErrorState
      Right _ -> do
        HH.footnote "Expected 'executeRow' to return failure, but it did not"
        HH.failure
