module Test.Transaction.Util
  ( TestError
  , throwTestError
  , silentlyHandleTestError
  , genNestingLevel
  , runNestedTransactions
  , runNestedTransactionWithInstructions
  , transationNestingLevelRange
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville

data TestError
  = TestError
  deriving (Show)

instance ExSafe.Exception TestError

runNestedTransactions ::
  Orville.MonadOrville m =>
  Int ->
  (Int -> m ()) ->
  m ()
runNestedTransactions maxLevel doLevel =
  let
    go is =
      case is of
        [] ->
          pure ()
        (item : rest) ->
          Orville.withTransaction $ do
            doLevel item
            go rest
  in
    go [1 .. maxLevel]

runNestedTransactionWithInstructions ::
  Orville.MonadOrville m =>
  Int ->
  (Int -> m Orville.TransactionInstruction) ->
  m ()
runNestedTransactionWithInstructions maxLevel doLevel =
  let
    go is =
      case is of
        [] ->
          pure ()
        (item : rest) ->
          Orville.withTransactionInstruction $ do
            instruction <- doLevel item
            go rest
            pure ((), instruction)
  in
    go [1 .. maxLevel]

throwTestError :: ExSafe.MonadThrow m => m a
throwTestError =
  ExSafe.throw TestError

silentlyHandleTestError :: ExSafe.MonadCatch m => m () -> m ()
silentlyHandleTestError =
  ExSafe.handle (\TestError -> pure ())

genNestingLevel :: HH.Gen Int
genNestingLevel =
  Gen.integral transationNestingLevelRange

transationNestingLevelRange :: HH.Range Int
transationNestingLevelRange =
  Range.linear 0 6
