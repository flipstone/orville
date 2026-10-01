module Test.Execution
  ( executionTests
  )
where

import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Test.Property as Property

executionTests :: Orville.ConnectionPool -> Tasty.TestTree
executionTests pool =
  Tasty.testGroup
    "Execution"
    [ TastyHH.testProperty
        "executeVoid makes execution callbacks"
        (prop_executeVoidCallbacks pool)
    , TastyHH.testProperty
        "executeAndDecode makes execution callbacks"
        (prop_executeAndDecodeCallbacks pool)
    , TastyHH.testProperty
        "executeAndReturnAffectedRows works as advertised"
        (prop_executeAndReturnAffectedRows pool)
    , TastyHH.testProperty
        "executeAndReturnAffectedRows makes execution callbacks"
        (prop_executeAndReturnAffectedRowsCallbacks pool)
    ]

prop_executeVoidCallbacks :: Orville.ConnectionPool -> HH.Property
prop_executeVoidCallbacks pool =
  Property.singletonProperty $ do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let
      selectOne =
        RawSql.fromString "SELECT 1 as number"

    HH.evalIO . Orville.runOrville pool $
      Orville.localOrvilleState
        ( Orville.addSqlExecutionCallback (appendTrace traceRef "Outer")
            . Orville.addSqlExecutionCallback (appendTrace traceRef "Inner")
        )
        (Orville.executeVoid Orville.SelectQuery selectOne)

    callbackTrace <- HH.evalIO $ IORef.readIORef traceRef
    callbackTrace
      === [ ("Outer", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          , ("Inner", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          ]

prop_executeAndDecodeCallbacks :: Orville.ConnectionPool -> HH.Property
prop_executeAndDecodeCallbacks pool =
  Property.singletonProperty $ do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let
      selectOne =
        RawSql.fromString "SELECT 1 as number"

      marshaller =
        Orville.annotateSqlMarshallerEmptyAnnotation $
          Orville.marshallField id (Orville.integerField "number")
    _ <-
      HH.evalIO . Orville.runOrville pool $
        Orville.localOrvilleState
          ( Orville.addSqlExecutionCallback (appendTrace traceRef "Outer")
              . Orville.addSqlExecutionCallback (appendTrace traceRef "Inner")
          )
          (Orville.executeAndDecode Orville.SelectQuery selectOne marshaller)

    callbackTrace <- HH.evalIO $ IORef.readIORef traceRef
    callbackTrace
      === [ ("Outer", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          , ("Inner", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          ]

prop_executeAndReturnAffectedRows :: Orville.ConnectionPool -> HH.Property
prop_executeAndReturnAffectedRows pool =
  Property.singletonProperty $ do
    let
      selectOne =
        RawSql.fromString "SELECT 1 as number"

    affectedRows <-
      HH.evalIO . Orville.runOrville pool $ do
        Orville.executeAndReturnAffectedRows Orville.UpdateQuery selectOne

    affectedRows === 1

prop_executeAndReturnAffectedRowsCallbacks :: Orville.ConnectionPool -> HH.Property
prop_executeAndReturnAffectedRowsCallbacks pool =
  Property.singletonProperty $ do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let
      selectOne =
        RawSql.fromString "SELECT 1 as number"

    _ <-
      HH.evalIO . Orville.runOrville pool $
        Orville.localOrvilleState
          ( Orville.addSqlExecutionCallback (appendTrace traceRef "Outer")
              . Orville.addSqlExecutionCallback (appendTrace traceRef "Inner")
          )
          (Orville.executeAndReturnAffectedRows Orville.SelectQuery selectOne)

    callbackTrace <- HH.evalIO $ IORef.readIORef traceRef
    callbackTrace
      === [ ("Outer", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          , ("Inner", Orville.SelectQuery, RawSql.toExampleBytes selectOne)
          ]

appendTrace ::
  IORef.IORef [(String, Orville.QueryType, BS.ByteString)] ->
  String ->
  Orville.QueryType ->
  RawSql.RawSql ->
  IO a ->
  IO a
appendTrace traceRef label queryType sql action = do
  let
    trace = (label, queryType, RawSql.toExampleBytes sql)
  IORef.modifyIORef traceRef (++ [trace])
  action
