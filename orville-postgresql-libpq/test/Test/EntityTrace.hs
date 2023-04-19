module Test.EntityTrace
  ( entityTraceTests,
  )
where

import qualified Control.Monad as Monad
import qualified Data.String as String
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.EntityTrace as EntityTrace

import qualified Test.Entities.Foo as Foo
import qualified Test.Property as Property
import qualified Test.Transaction.Util as TransactionUtil

entityTraceTests :: Orville.Pool Orville.Connection -> Property.Group
entityTraceTests pool =
  Property.group "EntityTrace" $
    [ prop_tracedOperationsAreReported pool
    , prop_tracesWithinCommittedTransactionsAreKept pool
    , prop_tracesWithinRolledBackTransactionsAreDropped pool
    , prop_tracesWithinRolledBackSavepointsAreDropped pool
    ]

data TestTrace
  = FooInserted Foo.Foo Foo.Foo
  | FooUpdated Foo.Foo Foo.Foo (Maybe Foo.Foo)
  | FooDeleted Foo.FooId (Maybe Foo.Foo)
  deriving (Eq, Show)

prop_tracedOperationsAreReported :: Property.NamedDBProperty
prop_tracedOperationsAreReported =
  Property.singletonNamedDBProperty "Traced operations are reported" $ \pool -> do
    fooId <- HH.forAll Foo.generateFooId
    initialFoo <- HH.forAll $ Foo.generateFooWithId fooId
    updatedFoo <- HH.forAll $ Foo.generateFooWithId fooId

    Foo.withTable pool (pure ())

    ((), fooTraces) <-
      HH.evalIO $ do
        EntityTrace.runEntityTraceT Orville.defaultErrorDetailLevel pool Orville.runOrvilleWithState $ do
          _ <- EntityTrace.insertEntityTraced tracedFooTable initialFoo
          _ <- EntityTrace.updateEntityTraced tracedFooTable initialFoo updatedFoo
          _ <- EntityTrace.deleteEntityTraced tracedFooTable fooId
          pure ()

    fooTraces
      === [ FooInserted initialFoo initialFoo
          , FooUpdated initialFoo updatedFoo (Just updatedFoo)
          , FooDeleted fooId (Just updatedFoo)
          ]

prop_tracesWithinCommittedTransactionsAreKept :: Property.NamedDBProperty
prop_tracesWithinCommittedTransactionsAreKept =
  Property.namedDBProperty "Traces within committed transactions are kept" $ \pool -> do
    foos <- HH.forAll $ Foo.generateList TransactionUtil.transationNestingLevelRange

    Foo.withTable pool (pure ())

    ((), fooTraces) <-
      HH.evalIO $ do
        EntityTrace.runEntityTraceT Orville.defaultErrorDetailLevel pool Orville.runOrvilleWithState $ do
          TransactionUtil.runNestedTransactionItems foos $ \foo ->
            Monad.void $ EntityTrace.insertEntityTraced tracedFooTable foo

    fooTraces === map (\foo -> FooInserted foo foo) foos

prop_tracesWithinRolledBackTransactionsAreDropped :: Property.NamedDBProperty
prop_tracesWithinRolledBackTransactionsAreDropped =
  Property.namedDBProperty "Traces within rolled-back transactions are dropped" $ \pool -> do
    foos <- HH.forAll $ Foo.generateList TransactionUtil.transationNestingLevelRange

    Foo.withTable pool (pure ())

    ((), fooTraces) <-
      HH.evalIO $ do
        EntityTrace.runEntityTraceT Orville.defaultErrorDetailLevel pool Orville.runOrvilleWithState $ do
          TransactionUtil.silentlyHandleTestError $
            TransactionUtil.runNestedTransactionItems foos $ \foo -> do
              Monad.void $ EntityTrace.insertEntityTraced tracedFooTable foo
              Monad.when (foo == last foos) TransactionUtil.throwTestError

    fooTraces === []

prop_tracesWithinRolledBackSavepointsAreDropped :: Property.NamedDBProperty
prop_tracesWithinRolledBackSavepointsAreDropped =
  Property.namedDBProperty "Traces within inner transactions that rollback are dropped (while outer committed transactions are kept)" $ \pool -> do
    foos <-
      HH.forAll $
        Foo.generateList (fmap (2 *) TransactionUtil.transationNestingLevelRange)

    outerLevels <-
      HH.forAll $
        Gen.frequency
          [ (1, pure 0)
          , (5, Gen.integral (Range.linear 0 (length foos)))
          , (1, pure (length foos))
          ]

    let (outerFoos, innerFoos) =
          splitAt outerLevels foos

        innerActions =
          TransactionUtil.runNestedTransactionItems innerFoos $ \foo -> do
            Monad.void $ EntityTrace.insertEntityTraced tracedFooTable foo
            Monad.when (foo == last innerFoos) TransactionUtil.throwTestError

        outerActions =
          if null outerFoos
            then TransactionUtil.silentlyHandleTestError innerActions
            else do
              TransactionUtil.runNestedTransactionItems outerFoos $ \foo -> do
                Monad.void $ EntityTrace.insertEntityTraced tracedFooTable foo
                Monad.when (foo == last outerFoos) $
                  TransactionUtil.silentlyHandleTestError innerActions

    Foo.withTable pool (pure ())

    ((), fooTraces) <-
      HH.evalIO $
        EntityTrace.runEntityTraceT Orville.defaultErrorDetailLevel pool Orville.runOrvilleWithState $ do
          outerActions

    let hasInner = not $ null innerFoos
        hasOuter = not $ null outerFoos

    HH.cover 5 (String.fromString "With inner transactions") hasInner
    HH.cover 5 (String.fromString "With outer transactions") hasOuter
    HH.cover 5 (String.fromString "With both inner and outer transactions") (hasInner && hasOuter)
    HH.cover 1 (String.fromString "With only inner transactions") (hasInner && not hasOuter)
    HH.cover 1 (String.fromString "With only outer transactions") (not hasInner && hasOuter)

    fooTraces === map (\foo -> FooInserted foo foo) outerFoos

tracedFooTable :: EntityTrace.TracedTable TestTrace Foo.FooId Foo.Foo Foo.Foo
tracedFooTable =
  EntityTrace.addDeleteTrace fooDeleteTrace
    . EntityTrace.addUpdateTrace fooUpdateTrace
    . EntityTrace.addInsertTrace fooInsertTrace
    . EntityTrace.mkTracedTable Foo.fooId
    $ Foo.table

fooInsertTrace :: Foo.Foo -> Foo.Foo -> [TestTrace]
fooInsertTrace writeFoo readFoo =
  [FooInserted writeFoo readFoo]

fooUpdateTrace :: Foo.Foo -> Foo.Foo -> Maybe Foo.Foo -> [TestTrace]
fooUpdateTrace readFoo writeFoo mbUpdatedFoo =
  [FooUpdated readFoo writeFoo mbUpdatedFoo]

fooDeleteTrace :: Foo.FooId -> Maybe Foo.Foo -> [TestTrace]
fooDeleteTrace fooId mbDeletedFoo =
  [FooDeleted fooId mbDeletedFoo]
