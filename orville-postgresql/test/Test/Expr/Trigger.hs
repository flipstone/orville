module Test.Expr.Trigger
  ( triggerTests
  ) where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.ByteString.Char8 as B8
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Test.Expr.TestSchema (assertEqualFooBarRows, dropAndRecreateTestTable, findAllFooBars, fooBarTable, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

triggerTests :: Orville.ConnectionPool -> Tasty.TestTree
triggerTests pool =
  Tasty.testGroup
    "Expr - Trigger"
    [ TastyHH.testProperty "creates a trigger on a table" (prop_triggers pool)
    , TastyHH.testProperty "dropTrigger renders the drop behavior" prop_dropTriggerSql
    ]

prop_dropTriggerSql :: HH.Property
prop_dropTriggerSql =
  Property.singletonProperty $
    RawSql.toExampleBytes (Expr.dropTrigger (Just Expr.ifExists) (Expr.triggerName "test_trigger") fooBarTable (Just Expr.cascade))
      === B8.pack "DROP TRIGGER IF EXISTS \"test_trigger\" ON \"foobar\" CASCADE"

prop_triggers :: Orville.ConnectionPool -> HH.Property
prop_triggers pool =
  Property.singletonProperty $ do
    let
      fooBars =
        pure $ mkFooBar 1 "dog"

      expectedFooBars =
        [mkFooBar 1 "god"]

      triggerBody =
        "BEGIN \
        \  NEW.bar = reverse (NEW.bar); \
        \  return NEW; \
        \END"

      triggerFunctionName =
        Expr.unqualified $ Expr.functionName "test_trigger_function"

      createTriggerFunction =
        Expr.createFunction
          (Just Expr.orReplace)
          triggerFunctionName
          []
          (Expr.returns Expr.returnTypeTrigger)
          (Expr.language Expr.plpgsql)
          (Expr.asDefinition triggerBody)

      createTrigger =
        Expr.createTrigger
          Nothing
          (Expr.triggerName "test_trigger")
          Expr.triggerBefore
          (pure Expr.triggerOnInsert)
          fooBarTable
          Expr.triggerForEachRow
          triggerFunctionName

    rows <-
      MIO.liftIO $
        Conn.withPoolConnection pool $ \connection -> do
          dropAndRecreateTestTable connection
          RawSql.executeVoid connection createTriggerFunction
          RawSql.executeVoid connection createTrigger
          RawSql.executeVoid connection $
            Expr.insertExpr fooBarTable Nothing (insertFooBarSource fooBars) Nothing Nothing

          result <- RawSql.execute connection findAllFooBars

          Execution.readRows result

    assertEqualFooBarRows rows expectedFooBars
