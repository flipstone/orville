module Test.Sequence
  ( sequenceTests,
  )
where

import qualified Control.Monad.IO.Class as MIO
import Hedgehog ((===))

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr

import qualified Test.Property as Property

sequenceTests :: Orville.Pool Orville.Connection -> Property.Group
sequenceTests pool =
  Property.group
    "Sequence"
    [ prop_nextValue pool
    , prop_currentValue pool
    , prop_setValue pool
    ]

prop_nextValue :: Property.NamedDBProperty
prop_nextValue =
  Property.singletonNamedDBProperty "Fetching the next value from a sequence" $ \pool -> do
    result <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          createTestSequence
          traverse (\() -> Orville.sequenceNextValue testSequence) [(), (), ()]
    result === [1, 2, 3]

prop_currentValue :: Property.NamedDBProperty
prop_currentValue =
  Property.singletonNamedDBProperty "Fetching the current value from a sequence" $ \pool -> do
    result <-
      MIO.liftIO $
        Orville.runOrville pool $ do
          createTestSequence
          -- get the next value once to initialize the sequence so that we can
          -- query the current value
          _ <- Orville.sequenceNextValue testSequence
          traverse (\() -> Orville.sequenceCurrentValue testSequence) [(), (), ()]
    result === [1, 1, 1]

prop_setValue :: Property.NamedDBProperty
prop_setValue =
  Property.singletonNamedDBProperty "Setting the current value of a sequence" $ \pool -> do
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
