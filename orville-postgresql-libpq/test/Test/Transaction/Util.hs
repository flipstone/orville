module Test.Transaction.Util
  ( TestError,
    throwTestError,
    silentlyHandleTestError,
    genNestingLevel,
    runNestedTransactions,
    runNestedTransactionItems,
    transationNestingLevelRange,
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
runNestedTransactions maxLevel =
  runNestedTransactionItems [1 .. maxLevel]

runNestedTransactionItems ::
  Orville.MonadOrville m =>
  [a] ->
  (a -> m ()) ->
  m ()
runNestedTransactionItems items doLevel =
  let go is =
        case is of
          [] ->
            pure ()
          (item : rest) ->
            Orville.withTransaction $ do
              doLevel item
              go rest
   in go items

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
