module Test.Transaction
  ( transactionTests,
  )
where

import qualified Control.Exception.Safe as ExSafe
import qualified Data.IORef as IORef
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState

import qualified Test.Property as Property
import qualified Test.TestTable as TestTable

transactionTests :: Pool.Pool Conn.Connection -> Property.Group
transactionTests pool =
  Property.group "Transaction" $
    [ prop_transactionsWithoutExceptionsCommit pool
    , prop_exceptionsLeadToTransactionRollback pool
    , prop_savepointsRollbackInnerTransactions pool
    , prop_callbacksMadeForTransactionCommit pool
    , prop_callbacksMadeForTransactionRollback pool
    ]

prop_transactionsWithoutExceptionsCommit :: Property.NamedDBProperty
prop_transactionsWithoutExceptionsCommit =
  Property.namedDBProperty "Transactions without exceptions perform a commit" $ \pool -> do
    nestingLevel <- HH.forAll genNestingLevel

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          withNestedTransactions nestingLevel $
            NestedActions
              { atEachLevel = Orville.insertEntity tracerTable Tracer
              , atInnermost = pure ()
              }

          Orville.findEntitiesBy tracerTable mempty

    length tracers === (nestingLevel + 1)

prop_exceptionsLeadToTransactionRollback :: Property.NamedDBProperty
prop_exceptionsLeadToTransactionRollback =
  Property.namedDBProperty "Exceptions within transaction blocks execute rollbock" $ \pool -> do
    nestingLevel <- HH.forAll genNestingLevel

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          ExSafe.handle (\TestError -> pure ()) $
            withNestedTransactions nestingLevel $
              NestedActions
                { atEachLevel = Orville.insertEntity tracerTable Tracer
                , atInnermost = ExSafe.throw TestError
                }

          Orville.findEntitiesBy tracerTable mempty

    length tracers === 0

prop_savepointsRollbackInnerTransactions :: Property.NamedDBProperty
prop_savepointsRollbackInnerTransactions =
  Property.namedDBProperty "Savepoints allow inner transactions to rollback while outer transactions commit" $ \pool -> do
    outerNestingLevel <- HH.forAll genNestingLevel
    innerNestingLevel <- HH.forAll genNestingLevel

    let doInnerTransactions =
          ExSafe.handle (\TestError -> pure ()) $
            withNestedTransactions innerNestingLevel $
              NestedActions
                { atEachLevel = Orville.insertEntity tracerTable Tracer
                , atInnermost = ExSafe.throw TestError
                }

        doTransactions =
          withNestedTransactions outerNestingLevel $
            NestedActions
              { atEachLevel = Orville.insertEntity tracerTable Tracer
              , atInnermost = doInnerTransactions
              }

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          doTransactions
          Orville.findEntitiesBy tracerTable mempty

    length tracers === (outerNestingLevel + 1)

prop_callbacksMadeForTransactionCommit :: Property.NamedDBProperty
prop_callbacksMadeForTransactionCommit =
  Property.namedDBProperty "Callbacks are delivered for a transaction that is commited" $ \pool -> do
    nestingLevel <- HH.forAll genNestingLevel

    allEvents <-
      captureTransactionCallbackEvents pool nestingLevel $
        NestedActions
          { atEachLevel = pure ()
          , atInnermost = pure ()
          }

    let expectedEvents =
          mkExpectedEventsForNestingLevel nestingLevel $ \maybeSavepoint ->
            case maybeSavepoint of
              Nothing -> (Orville.BeginTransaction, Orville.CommitTransaction)
              Just savepoint -> (Orville.NewSavepoint savepoint, Orville.ReleaseSavepoint savepoint)

    allEvents === expectedEvents

prop_callbacksMadeForTransactionRollback :: Property.NamedDBProperty
prop_callbacksMadeForTransactionRollback =
  Property.namedDBProperty "Callbacks are delivered for a transaction this is rolled back" $ \pool -> do
    nestingLevel <- HH.forAll genNestingLevel

    allEvents <-
      captureTransactionCallbackEvents pool nestingLevel $
        NestedActions
          { atEachLevel = pure ()
          , atInnermost = ExSafe.throw TestError
          }

    let expectedEvents =
          mkExpectedEventsForNestingLevel nestingLevel $ \maybeSavepoint ->
            case maybeSavepoint of
              Nothing -> (Orville.BeginTransaction, Orville.RollbackTransaction)
              Just savepoint -> (Orville.NewSavepoint savepoint, Orville.RollbackToSavepoint savepoint)

    allEvents === expectedEvents

data NestedActions = NestedActions
  { atEachLevel :: Orville.Orville ()
  , atInnermost :: Orville.Orville ()
  }

withNestedTransactions ::
  Int ->
  NestedActions ->
  Orville.Orville ()
withNestedTransactions nestingLevel nestedActions =
  Orville.withTransaction $ do
    atEachLevel nestedActions
    if nestingLevel == 0
      then atInnermost nestedActions
      else withNestedTransactions (nestingLevel - 1) nestedActions

captureTransactionCallbackEvents ::
  Pool.Pool Conn.Connection ->
  Int ->
  NestedActions ->
  HH.PropertyT IO [Orville.TransactionEvent]
captureTransactionCallbackEvents pool nestingLevel nestedActions = do
  callbackEventsRef <- HH.evalIO $ IORef.newIORef []

  let captureEvent event =
        IORef.modifyIORef callbackEventsRef (event :)

      addEventCaptureCallback =
        Orville.addTransactionCallback captureEvent

  HH.evalIO $ do
    Orville.runOrville pool $ do
      ExSafe.handle (\TestError -> pure ()) $ do
        Orville.localOrvilleState addEventCaptureCallback $ do
          withNestedTransactions nestingLevel nestedActions

    reverse <$> IORef.readIORef callbackEventsRef

mkExpectedEventsForNestingLevel ::
  Int ->
  (Maybe Orville.Savepoint -> (Orville.TransactionEvent, Orville.TransactionEvent)) ->
  [Orville.TransactionEvent]
mkExpectedEventsForNestingLevel nestingLevel mkEventsForLevel =
  let go remainingSavepointsInnerFirst befores afters =
        let (newBeforeEvent, newAfterEvent) =
              mkEventsForLevel $
                case remainingSavepointsInnerFirst of
                  [] -> Nothing
                  savepoint : _ -> Just savepoint

            newBefores = newBeforeEvent : befores
            newAfters = newAfterEvent : afters
         in case remainingSavepointsInnerFirst of
              [] ->
                -- The outermost after event is at the head of the list and should
                -- be the _last_ event that is delived by Orville so we reverse the
                -- afters event list to construct the list of events in the correct
                -- expected order
                newBefores ++ reverse newAfters
              _ : rest ->
                go rest newBefores newAfters

      -- A list of all savepoints we expected Orville to create based on the
      -- nesting level. The list is ordered with the inner-most savepoint first
      -- to make it easier to build the event list from the inside out
      savepointsInnerFirst =
        reverse $
          take
            nestingLevel
            (iterate OrvilleState.nextSavepoint OrvilleState.initialSavepoint)
   in go savepointsInnerFirst [] []

genNestingLevel :: HH.Gen Int
genNestingLevel =
  Gen.integral $ Range.linear 0 5

data Tracer
  = Tracer

tracerTable :: Orville.TableDefinition Orville.NoKey Tracer Tracer
tracerTable =
  Orville.mkTableDefinitionWithoutKey "tracer" tracerMarshaller

tracerMarshaller :: Orville.SqlMarshaller Tracer Tracer
tracerMarshaller =
  const Tracer
    <$> Orville.marshallField (const $ T.pack "tracer") (Orville.unboundedTextField "tracer")

data TestError
  = TestError
  deriving (Show)

instance ExSafe.Exception TestError
