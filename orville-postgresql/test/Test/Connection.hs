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
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue

import qualified Test.PgGen as PgGen
import qualified Test.Property as Property

connectionTests :: Conn.ConnectionPool -> Tasty.TestTree
connectionTests pool =
  Tasty.testGroup
    "Connection"
    [ TastyHH.testProperty
        "executeRaw can pass non-null bytes equivalents whether checked for NUL or not"
        (prop_safeOrUnsafeNonNullBytes pool)
    , TastyHH.testProperty
        "executeRaw returns error if nul byte is given using safe constructor"
        (prop_errorOnSafeNulByte pool)
    , TastyHH.testProperty
        "executeRaw truncates values at the nul byte given using unsafe constructor"
        (prop_truncateValuesAtUnsafeNulByte pool)
    , TastyHH.testProperty
        "executeRaw returns error if invalid sql is given"
        (prop_errorOnInvalidSql pool)
    , TastyHH.testProperty
        "returning a connection to the pool with an open transaction causes an exception"
        (prop_returningAConnectionToPoolWithOpenTransactionCausesException pool)
    , TastyHH.testProperty
        "destroying idle connections in the pool does not affect connections already acquired"
        (prop_acquiredConnectionNotDestroyed pool)
    , TastyHH.testProperty
        "the connection pool can be used after destroying idle connections"
        (prop_poolUsableAfterDestroyingConnections pool)
    ]

prop_safeOrUnsafeNonNullBytes :: Conn.ConnectionPool -> HH.Property
prop_safeOrUnsafeNonNullBytes pool =
  HH.property $ do
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

prop_errorOnSafeNulByte :: Conn.ConnectionPool -> HH.Property
prop_errorOnSafeNulByte pool =
  HH.property $ do
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

prop_truncateValuesAtUnsafeNulByte :: Conn.ConnectionPool -> HH.Property
prop_truncateValuesAtUnsafeNulByte pool =
  HH.property $ do
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

prop_errorOnInvalidSql :: Conn.ConnectionPool -> HH.Property
prop_errorOnInvalidSql pool =
  Property.singletonProperty $ do
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
        HH.footnote "Expected 'executeRaw' to return failure, but it did not"
        HH.failure

prop_returningAConnectionToPoolWithOpenTransactionCausesException :: Conn.ConnectionPool -> HH.Property
prop_returningAConnectionToPoolWithOpenTransactionCausesException pool =
  Property.singletonProperty $ do
    result <-
      MIO.liftIO . E.try . Conn.withPoolConnection pool $ \connection ->
        Conn.executeRaw
          connection
          (B8.pack "BEGIN TRANSACTION")
          []

    case result of
      Left (_err :: Conn.ConnectionError) ->
        pure ()
      Right _ -> do
        HH.footnote "Expected 'withPoolConnection' to return failure, but it did not"
        HH.failure

prop_acquiredConnectionNotDestroyed :: Conn.ConnectionPool -> HH.Property
prop_acquiredConnectionNotDestroyed pool =
  HH.property $ do
    textBytes <- fmap Enc.encodeUtf8 . HH.forAll $ PgGen.pgText (Range.linear 0 256)

    value <-
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        Conn.destroyIdleConnections pool
        result <-
          Conn.executeRaw
            connection
            (B8.pack "SELECT $1::text")
            [ Just $ PgTextFormatValue.fromByteString textBytes
            ]

        LibPQ.getvalue' result 0 0

    value === Just textBytes

prop_poolUsableAfterDestroyingConnections :: Conn.ConnectionPool -> HH.Property
prop_poolUsableAfterDestroyingConnections pool =
  HH.property $ do
    textBytes1 <- fmap Enc.encodeUtf8 . HH.forAll $ PgGen.pgText (Range.linear 0 256)
    textBytes2 <- fmap Enc.encodeUtf8 . HH.forAll $ PgGen.pgText (Range.linear 0 256)

    value1 <-
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        result <-
          Conn.executeRaw
            connection
            (B8.pack "SELECT $1::text")
            [ Just $ PgTextFormatValue.fromByteString textBytes1
            ]

        LibPQ.getvalue' result 0 0

    MIO.liftIO $ Conn.destroyIdleConnections pool

    value2 <-
      MIO.liftIO . Conn.withPoolConnection pool $ \connection -> do
        result <-
          Conn.executeRaw
            connection
            (B8.pack "SELECT $1::text")
            [ Just $ PgTextFormatValue.fromByteString textBytes2
            ]

        LibPQ.getvalue' result 0 0

    value1 === Just textBytes1
    value2 === Just textBytes2
