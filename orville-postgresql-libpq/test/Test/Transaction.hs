module Test.Transaction
  ( transactionTests,
  )
where

import qualified Control.Monad as Monad
import qualified Data.ByteString as BS
import qualified Data.IORef as IORef
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property
import qualified Test.TestTable as TestTable
import qualified Test.Transaction.Util as TransactionUtil

transactionTests :: Orville.Pool Orville.Connection -> Property.Group
transactionTests pool =
  Property.group "Transaction" $
    [ prop_transactionsWithoutExceptionsCommit pool
    , prop_exceptionsLeadToTransactionRollback pool
    , prop_savepointsRollbackInnerTransactions pool
    , prop_callbacksMadeForTransactionCommit pool
    , prop_callbacksMadeForTransactionRollback pool
    , prop_usesCustomBeginTransactionSql pool
    ]

prop_transactionsWithoutExceptionsCommit :: Property.NamedDBProperty
prop_transactionsWithoutExceptionsCommit =
  Property.namedDBProperty "Transactions without exceptions perform a commit" $ \pool -> do
    nestingLevel <- HH.forAll TransactionUtil.genNestingLevel

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          TransactionUtil.runNestedTransactions nestingLevel $ \_ ->
            Monad.void $ Orville.insertEntity tracerTable Tracer
          Orville.findEntitiesBy tracerTable mempty

    length tracers === nestingLevel

prop_exceptionsLeadToTransactionRollback :: Property.NamedDBProperty
prop_exceptionsLeadToTransactionRollback =
  Property.namedDBProperty "Exceptions within transaction blocks execute rollbock" $ \pool -> do
    nestingLevel <- HH.forAll TransactionUtil.genNestingLevel

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          TransactionUtil.silentlyHandleTestError $
            TransactionUtil.runNestedTransactions nestingLevel $ \level -> do
              _ <- Orville.insertEntity tracerTable Tracer
              Monad.when (level >= nestingLevel) TransactionUtil.throwTestError

          Orville.findEntitiesBy tracerTable mempty

    length tracers === 0

prop_savepointsRollbackInnerTransactions :: Property.NamedDBProperty
prop_savepointsRollbackInnerTransactions =
  Property.namedDBProperty "Savepoints allow inner transactions to rollback while outer transactions commit" $ \pool -> do
    outerNestingLevel <- HH.forAll TransactionUtil.genNestingLevel
    innerNestingLevel <- HH.forAll TransactionUtil.genNestingLevel

    let innerActions =
          TransactionUtil.runNestedTransactions innerNestingLevel $ \level -> do
            _ <- Orville.insertEntity tracerTable Tracer
            Monad.when (level >= innerNestingLevel) TransactionUtil.throwTestError

        outerActions =
          TransactionUtil.runNestedTransactions outerNestingLevel $ \level -> do
            _ <- Orville.insertEntity tracerTable Tracer
            Monad.when (level >= outerNestingLevel) $
              TransactionUtil.silentlyHandleTestError innerActions

    tracers <-
      HH.evalIO $ do
        Pool.withResource pool $ \connection ->
          TestTable.dropAndRecreateTableDef connection tracerTable

        Orville.runOrville pool $ do
          outerActions
          Orville.findEntitiesBy tracerTable mempty

    length tracers === outerNestingLevel

prop_callbacksMadeForTransactionCommit :: Property.NamedDBProperty
prop_callbacksMadeForTransactionCommit =
  Property.namedDBProperty "Callbacks are delivered for a transaction that is commited" $ \pool -> do
    nestingLevel <- HH.forAll TransactionUtil.genNestingLevel

    allEvents <-
      captureTransactionCallbackEvents pool $
        TransactionUtil.runNestedTransactions nestingLevel (\_ -> pure ())

    let expectedEvents =
          mkExpectedEventsForNestedActions nestingLevel $ \maybeSavepoint ->
            case maybeSavepoint of
              Nothing -> (Orville.BeginTransaction, Orville.CommitTransaction)
              Just savepoint -> (Orville.NewSavepoint savepoint, Orville.ReleaseSavepoint savepoint)

    allEvents === expectedEvents

prop_callbacksMadeForTransactionRollback :: Property.NamedDBProperty
prop_callbacksMadeForTransactionRollback =
  Property.namedDBProperty "Callbacks are delivered for a transaction this is rolled back" $ \pool -> do
    nestingLevel <- HH.forAll TransactionUtil.genNestingLevel

    allEvents <- captureTransactionCallbackEvents pool $
      TransactionUtil.runNestedTransactions nestingLevel $ \level ->
        Monad.when (level >= nestingLevel) (TransactionUtil.throwTestError)

    let expectedEvents =
          mkExpectedEventsForNestedActions nestingLevel $ \maybeSavepoint ->
            case maybeSavepoint of
              Nothing -> (Orville.BeginTransaction, Orville.RollbackTransaction)
              Just savepoint -> (Orville.NewSavepoint savepoint, Orville.RollbackToSavepoint savepoint)

    allEvents === expectedEvents

prop_usesCustomBeginTransactionSql :: Property.NamedDBProperty
prop_usesCustomBeginTransactionSql =
  Property.namedDBProperty "Uses custom begin transaction sql" $ \pool -> do
    customExpr <-
      HH.forAllWith (show . RawSql.toExampleBytes) $
        Gen.element
          [ Expr.beginTransaction Nothing
          , Expr.beginTransaction (Just Expr.readOnly)
          , Expr.beginTransaction (Just Expr.readWrite)
          , Expr.beginTransaction (Just Expr.deferrable)
          , Expr.beginTransaction (Just Expr.notDeferrable)
          , Expr.beginTransaction (Just (Expr.isolationLevel Expr.serializable))
          , Expr.beginTransaction (Just (Expr.isolationLevel Expr.repeatableRead))
          , Expr.beginTransaction (Just (Expr.isolationLevel Expr.readCommitted))
          , Expr.beginTransaction (Just (Expr.isolationLevel Expr.readUncommitted))
          ]

    sqlTrace <-
      captureSqlTrace pool $ do
        Orville.localOrvilleState
          (Orville.setBeginTransactionExpr customExpr)
          (Orville.withTransaction $ pure ())

    sqlTrace
      === [ (Orville.OtherQuery, RawSql.toExampleBytes Expr.commit)
          , (Orville.OtherQuery, RawSql.toExampleBytes customExpr)
          ]

captureTransactionCallbackEvents ::
  Orville.Pool Orville.Connection ->
  Orville.Orville () ->
  HH.PropertyT IO [Orville.TransactionEvent]
captureTransactionCallbackEvents pool actions = do
  callbackEventsRef <- HH.evalIO $ IORef.newIORef []

  let captureEvent event =
        IORef.modifyIORef callbackEventsRef (event :)

      addEventCaptureCallback =
        Orville.addTransactionCallback captureEvent

  HH.evalIO $ do
    Orville.runOrville pool $
      TransactionUtil.silentlyHandleTestError $
        Orville.localOrvilleState addEventCaptureCallback actions

    reverse <$> IORef.readIORef callbackEventsRef

mkExpectedEventsForNestedActions ::
  Int ->
  (Maybe Orville.Savepoint -> (Orville.TransactionEvent, Orville.TransactionEvent)) ->
  [Orville.TransactionEvent]
mkExpectedEventsForNestedActions nestingLevel mkEventsForLevel =
  let appendEvents mbSavepoint (befores, afters) =
        let (before, after) = mkEventsForLevel mbSavepoint
         in (before : befores, after : afters)

      savepoints =
        iterate OrvilleState.nextSavepoint OrvilleState.initialSavepoint

      (allBefores, allAfters) =
        foldr appendEvents ([], []) $
          take nestingLevel (Nothing : map Just savepoints)
   in allBefores ++ reverse allAfters

data Tracer
  = Tracer

tracerTable :: Orville.TableDefinition Orville.NoKey Tracer Tracer
tracerTable =
  Orville.mkTableDefinitionWithoutKey "tracer" tracerMarshaller

tracerMarshaller :: Orville.SqlMarshaller Tracer Tracer
tracerMarshaller =
  const Tracer
    <$> Orville.marshallField (const $ T.pack "tracer") (Orville.unboundedTextField "tracer")

captureSqlTrace ::
  Orville.Pool Orville.Connection ->
  Orville.Orville () ->
  HH.PropertyT IO [(Orville.QueryType, BS.ByteString)]
captureSqlTrace pool actions = do
  queryTraceRef <- HH.evalIO $ IORef.newIORef []

  let captureQuery :: Orville.QueryType -> RawSql.RawSql -> IO a -> IO a
      captureQuery queryType sql action = do
        IORef.modifyIORef queryTraceRef ((queryType, RawSql.toExampleBytes sql) :)
        action

  HH.evalIO $ do
    Orville.runOrville pool $
      Orville.localOrvilleState
        (Orville.addSqlExecutionCallback captureQuery)
        actions

    IORef.readIORef queryTraceRef
