module Test.Execution
  ( executionTests,
  )
where

import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import qualified Data.Pool as Pool
import Hedgehog ((===))
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Test.Property as Property

executionTests :: Pool.Pool Conn.Connection -> Property.Group
executionTests pool =
  Property.group
    "Execution"
    [ prop_executeVoidCallbacks pool
    , prop_executeAndDecodeCallbacks pool
    , prop_executeAndReturnAffectedRows pool
    , prop_executeAndReturnAffectedRowsCallbacks pool
    ]

prop_executeVoidCallbacks :: Property.NamedDBProperty
prop_executeVoidCallbacks =
  Property.singletonNamedDBProperty "exceuteVoid makes execution callbacks" $ \pool -> do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let selectOne =
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

prop_executeAndDecodeCallbacks :: Property.NamedDBProperty
prop_executeAndDecodeCallbacks =
  Property.singletonNamedDBProperty "exceuteAndDecode makes execution callbacks" $ \pool -> do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let selectOne =
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

prop_executeAndReturnAffectedRows :: Property.NamedDBProperty
prop_executeAndReturnAffectedRows =
  Property.singletonNamedDBProperty "executeAndReturnAffectedRows works as advertised" $ \pool -> do
    let selectOne =
          RawSql.fromString "SELECT 1 as number"

    affectedRows <-
      HH.evalIO . Orville.runOrville pool $ do
        Orville.executeAndReturnAffectedRows Orville.UpdateQuery selectOne

    affectedRows === 1

prop_executeAndReturnAffectedRowsCallbacks :: Property.NamedDBProperty
prop_executeAndReturnAffectedRowsCallbacks =
  Property.singletonNamedDBProperty "executeAndReturnAffectedRows makes execution callbacks" $ \pool -> do
    traceRef <- HH.evalIO $ IORef.newIORef []

    let selectOne =
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
  let trace = (label, queryType, RawSql.toExampleBytes sql)
  IORef.modifyIORef traceRef (++ [trace])
  action
