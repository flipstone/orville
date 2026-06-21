module Test.Sequence
  ( sequenceTests
  )
where

import qualified Control.Monad.IO.Class as MIO
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.Property as Property

sequenceTests :: Orville.ConnectionPool -> Tasty.TestTree
sequenceTests pool =
  Tasty.testGroup
    "Sequence"
    [ TastyHH.testProperty "Fetching the next value from a sequence" (prop_nextValue pool)
    , TastyHH.testProperty "Fetching the current value from a sequence" (prop_currentValue pool)
    , TastyHH.testProperty "Setting the current value of a sequence" (prop_setValue pool)
    ]

prop_nextValue :: Orville.ConnectionPool -> HH.Property
prop_nextValue pool =
  Property.singletonProperty $ do
    result <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          createTestSequence
          traverse (\() -> Orville.sequenceNextValue testSequence) [(), (), ()]
    result === [1, 2, 3]

prop_currentValue :: Orville.ConnectionPool -> HH.Property
prop_currentValue pool =
  Property.singletonProperty $ do
    result <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          createTestSequence
          -- get the next value once to initialize the sequence so that we can
          -- query the current value
          _ <- Orville.sequenceNextValue testSequence
          traverse (\() -> Orville.sequenceCurrentValue testSequence) [(), (), ()]
    result === [1, 1, 1]

prop_setValue :: Orville.ConnectionPool -> HH.Property
prop_setValue pool =
  Property.singletonProperty $ do
    result <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          createTestSequence
          _ <- Orville.sequenceSetValue testSequence 42
          Orville.sequenceCurrentValue testSequence
    result === 42

testSequence :: Orville.SequenceDefinition
testSequence =
  Orville.mkSequenceDefinition sequenceNameString

sequenceNameString :: String
sequenceNameString =
  "sequence_definition_test"

createTestSequence :: Orville.Orville ()
createTestSequence = do
  Orville.executeVoid Orville.DDLQuery $ Expr.dropSequenceExpr (Just Expr.ifExists) (Orville.sequenceName testSequence)
  Orville.executeVoid Orville.DDLQuery $ Orville.mkCreateSequenceExpr testSequence
