module Test.Expr.Trigger
  ( triggerTests
  ) where

import qualified Control.Monad.IO.Class as MIO
import Data.List.NonEmpty (NonEmpty ((:|)))

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.Connection as Conn
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import Test.Expr.TestSchema (assertEqualFooBarRows, dropAndRecreateTestTable, findAllFooBars, fooBarTable, insertFooBarSource, mkFooBar)
import qualified Test.Property as Property

triggerTests :: Orville.ConnectionPool -> Property.Group
triggerTests pool =
  Property.group
    "Expr - Trigger"
    [ prop_triggers pool
    ]

prop_triggers :: Property.NamedDBProperty
prop_triggers =
  Property.singletonNamedDBProperty "creates a trigger on a table" $ \pool -> do
    let
      fooBars =
        [mkFooBar 1 "dog"]

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
          (Expr.returns Expr.returnTypeTrigger)
          (Expr.language Expr.plpgsql)
          (Expr.asDefinition triggerBody)

      createTrigger =
        Expr.createTrigger
          Nothing
          (Expr.triggerName "test_trigger")
          Expr.triggerBefore
          (Expr.triggerOnInsert :| [])
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
