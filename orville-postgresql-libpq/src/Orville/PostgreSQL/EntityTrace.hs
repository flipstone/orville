{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Run various actions with user supplied tracing.

@since 0.10.0.0
-}
module Orville.PostgreSQL.EntityTrace
  ( EntityTraceT,
    EntityTraceState,
    runEntityTraceT,
    insertEntityTraced,
    updateEntityTraced,
    deleteEntityTraced,
    TracedTable,
    mkTracedTable,
    addInsertTrace,
    addUpdateTrace,
    addDeleteTrace,
    untracedTableDefinition,
    MonadEntityTrace (recordTraces, liftOrvilleUntraced),
  )
where

import qualified Control.Exception as Ex
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.DList as DList
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

import qualified Orville.PostgreSQL as O

{- |
 'MonadEntityTrace' provides the interface used by functions such as
 'insertEntityTraced' to perform tracing as part of their operation. An
 instance for 'ReaderT EntityTraceState' is provided which can be used via
 'runEntityTraceT'.

@since 0.10.0.0
-}
class Monad m => MonadEntityTrace trace m | m -> trace where
  recordTraces :: [trace] -> m ()
  liftOrvilleUntraced :: (forall n. O.MonadOrville n => n a) -> m a

{- |
  INTERNAL: A list of functions for constructing traces
-}
newtype Constructors f
  = Constructors (DList.DList f)

emptyConstructors :: Constructors f
emptyConstructors =
  Constructors DList.empty

addConstructor :: f -> Constructors f -> Constructors f
addConstructor f (Constructors fs) =
  Constructors (DList.snoc fs f)

applyConstructors :: (f -> [a]) -> Constructors f -> [a]
applyConstructors runF (Constructors fs) =
  concatMap runF fs

{- |
  A 'TracedTable' is a regular 'O.TableDefinition' with callbacks for
  constructing the @trace@ items that will be captured when functions such as
  'insertEntityTraced' are used. Use 'mkTracedTable' to build a 'TracedTable'
  with no callbacks and then use 'addInsertTrace' and friends to register
  callbacks callbacks on the table.

@since 0.10.0.0
-}
data TracedTable trace key writeEntity readEntity = TracedTable
  { _untracedTableDefinition :: O.TableDefinition (O.HasKey key) writeEntity readEntity
  , _getTracedEntityKey :: readEntity -> key
  , _insertTraceConstructors :: Constructors (writeEntity -> readEntity -> [trace])
  , _updateTraceConstructors :: Constructors (readEntity -> writeEntity -> Maybe readEntity -> [trace])
  , _deleteTraceConstructors :: Constructors (key -> Maybe readEntity -> [trace])
  }

{- |
  Constructs a 'TracedTable' that wraps the given 'O.TableDefinition' so that
  it can be used with functions such as 'insertEntityTraced'. The resulting
  'TracedTable' has no trace callbacks initially. You can 'addInsertTrace' and
  similar functions that add callbacks.

@since 0.10.0.0
-}
mkTracedTable ::
  (readEntity -> key) ->
  O.TableDefinition (O.HasKey key) writeEntity readEntity ->
  TracedTable trace key writeEntity readEntity
mkTracedTable getEntityKey tableDef =
  TracedTable
    { _untracedTableDefinition = tableDef
    , _getTracedEntityKey = getEntityKey
    , _insertTraceConstructors = emptyConstructors
    , _updateTraceConstructors = emptyConstructors
    , _deleteTraceConstructors = emptyConstructors
    }

{- |
  INTERNAL: Construct traces to be recorded by 'insertEntityTraced' based on
  all the constructor callbacks there were added via 'addInsertTrace'
-}
mkInsertTraces ::
  TracedTable trace key writeEntity readEntity ->
  writeEntity ->
  readEntity ->
  [trace]
mkInsertTraces tracedTable readEntity writeEntity =
  applyConstructors
    (\f -> f readEntity writeEntity)
    (_insertTraceConstructors tracedTable)

{- |
  INTERNAL: Construct traces to be recorded by 'updateEntityTraced' based on
  all the constructor callbacks there were added via 'addUpdateTrace'
-}
mkUpdateTraces ::
  TracedTable trace key writeEntity readEntity ->
  readEntity ->
  writeEntity ->
  Maybe readEntity ->
  [trace]
mkUpdateTraces tracedTable readEntity writeEntity mbUpdatedEntity =
  applyConstructors
    (\f -> f readEntity writeEntity mbUpdatedEntity)
    (_updateTraceConstructors tracedTable)

{- |
  INTERNAL: Construct traces to be recorded by 'deleteEntityTraced' based on
  all the constructor callbacks that were added via 'addDeleteTrace'
-}
mkDeleteTraces ::
  TracedTable trace key writeEntity readEntity ->
  key ->
  Maybe readEntity ->
  [trace]
mkDeleteTraces tracedTable key mbDeletedEntity =
  applyConstructors
    (\f -> f key mbDeletedEntity)
    (_deleteTraceConstructors tracedTable)

{- |
  Returns the 'O.TableDefinition' underlying a 'TracedTable'. You can use this
  to get access to the table definiton to use the regular untraced Orville
  functions without having to keep a separate reference to the untraced table
  around.

@since 0.10.0.0
-}
untracedTableDefinition ::
  TracedTable trace key writeEntity readEntity ->
  O.TableDefinition (O.HasKey key) writeEntity readEntity
untracedTableDefinition =
  _untracedTableDefinition

{- |
  Adds a callback that will be used during 'insertEntityTraced' to build the
  @trace@ items to be recorded for that insert. The trace building function
  will be passed the @writeEntity@ given to 'insertEntityTraced' as well as
  the @readEntity@ that is returned as a result of the insert.

@since 0.10.0.0
-}
addInsertTrace ::
  (writeEntity -> readEntity -> [trace]) ->
  TracedTable trace key writeEntity readEntity ->
  TracedTable trace key writeEntity readEntity
addInsertTrace mkTraces tracedTable =
  tracedTable
    { _insertTraceConstructors = addConstructor mkTraces (_insertTraceConstructors tracedTable)
    }

{- |
  Adds a callback that will be used during 'updateEntityTraced' to build the
  @trace@ items to be recorded for that update. The trace building function
  will be passed the @readEntity@ and @writeEntity@ that were given to
  'updateEntityTraced' as well as the any @readEntity@ that is returned as a
  result of the insert. If the update matches no rows in the database, 'Nothing'
  will be passed as the last argument to the trace building function.

@since 0.10.0.0
-}
addUpdateTrace ::
  (readEntity -> writeEntity -> Maybe readEntity -> [trace]) ->
  TracedTable trace key writeEntity readEntity ->
  TracedTable trace key writeEntity readEntity
addUpdateTrace mkTraces tracedTable =
  tracedTable
    { _updateTraceConstructors = addConstructor mkTraces (_updateTraceConstructors tracedTable)
    }

{- |
  Adds a callback that will be used during 'deleteEntityTraced' to build the
  @trace@ items to be recorded for that update. The trace building function
  will be passed the @key@ that sa given to 'deleteEntityTraced' as well as the
  any @readEntity@ that is returned as a result of the delete. If the delete
  matches no rows in the database, 'Nothing' will be passed as the last
  argument to the trace building function.

@since 0.10.0.0
-}
addDeleteTrace ::
  (key -> Maybe readEntity -> [trace]) ->
  TracedTable trace key writeEntity readEntity ->
  TracedTable trace key writeEntity readEntity
addDeleteTrace mkTraces tracedTable =
  tracedTable
    { _deleteTraceConstructors = addConstructor mkTraces (_deleteTraceConstructors tracedTable)
    }

{- |
  Inserts an entity into the database via 'O.insertAndReturnEntity', then records
  any traces returned by the 'addInsertTrace' callbacks on the 'TracedTable'
  via 'recordTraces'.

  See 'runEntityTraceT' for a stock way to run actions using these trace
  operations and get access to the traces that are recorded.

@since 0.10.0.0
-}
insertEntityTraced ::
  MonadEntityTrace trace m =>
  TracedTable trace key writeEntity readEntity ->
  writeEntity ->
  m readEntity
insertEntityTraced tracedTable writeEntity = do
  readEntity <-
    liftOrvilleUntraced $
      O.insertAndReturnEntity
        (_untracedTableDefinition tracedTable)
        writeEntity

  recordTraces $ mkInsertTraces tracedTable writeEntity readEntity
  pure readEntity

{- |
  Updates an entity in the database via 'O.updateAndReturnEntity', then records
  any traces returned by the 'addUpdateTrace' callbacks on the 'TracedTable'
  via 'recordTraces'.

  See 'runEntityTraceT' for a stock way to run actions using these trace
  operations and get access to the traces that are recorded.

@since 0.10.0.0
-}
updateEntityTraced ::
  MonadEntityTrace trace m =>
  TracedTable trace key writeEntity readEntity ->
  readEntity ->
  writeEntity ->
  m ()
updateEntityTraced tracedTable oldEntity newEntity = do
  updatedEntity <-
    liftOrvilleUntraced $
      O.updateAndReturnEntity
        (_untracedTableDefinition tracedTable)
        (_getTracedEntityKey tracedTable oldEntity)
        newEntity

  recordTraces $ mkUpdateTraces tracedTable oldEntity newEntity updatedEntity

{- |
  Updates an entity in the database via 'O.deleteAndReturnEntity', then records
  any traces returned by the 'addDeleteTrace' callbacks on the 'TracedTable'
  via 'recordTraces'.

  See 'runEntityTraceT' for a stock way to run actions using these trace
  operations and get access to the traces that are recorded.

@since 0.10.0.0
-}
deleteEntityTraced ::
  MonadEntityTrace trace m =>
  TracedTable trace key writeEntity readEntity ->
  key ->
  m ()
deleteEntityTraced tracedTable key = do
  readEntity <-
    liftOrvilleUntraced $
      O.deleteAndReturnEntity
        (_untracedTableDefinition tracedTable)
        key

  recordTraces $ mkDeleteTraces tracedTable key readEntity

{- |
  An 'EntityTraceState' is created by 'runEntityTraceT' to keep track of
  the state of traces that are to be returned when the function finishes.

@since 0.10.0.0
-}
newtype EntityTraceState trace = EntityTraceState
  { _entityTraceStateRef :: IORef (RecordedTraces trace)
  }

{- |
  INTERNAL: The 'RecordedTraces' structure stores all the data necessary to
  keep the list of traces to be returned by 'runEntityTraceT' in sync with the
  state of committed and rolled-back transactions that happen at the database
  level, including savepoints. The 'EntityTraceState' keeps track of the
  stricture in an 'IORef' that is modified over the life of the action that is
  run by 'runEntityTraceT'
-}
data RecordedTraces trace = RecordedTraces
  { committedTracesDList :: DList.DList trace
  , currentTransactionStack :: Maybe (TransactionStack trace)
  }

{- |
  INTERNAL: Tracks the state of any traces that occur while a transaction is
  open. These are kept in a stack so that savepoints can be created, released
  and rolled-back in sync with any `O.withTransaction` operations that occur
  during 'runEntityTraceT'.
-}
data TransactionStack trace
  = TransactionOpened (DList.DList trace)
  | SavepointCreated O.Savepoint (DList.DList trace) (TransactionStack trace)

{- |
  INTERNAL: Assembles all the traces for the 'TransactionStack' into a 'DList'
  in the order in which they originally occurred
-}
allTransactionStackTraces :: TransactionStack trace -> DList.DList trace
allTransactionStackTraces stack =
  case stack of
    TransactionOpened traces ->
      traces
    SavepointCreated _ traces remainingStack ->
      allTransactionStackTraces remainingStack <> traces

{- |
  INTERNAL: Adds some traces to whatever the currently open (innermost)
  transaction or savepoint is for this stack.
-}
appendTracesAtTopOfStack ::
  DList.DList trace ->
  TransactionStack trace ->
  TransactionStack trace
appendTracesAtTopOfStack newTraces stack =
  case stack of
    TransactionOpened traces ->
      TransactionOpened (traces <> newTraces)
    SavepointCreated savepoint traces remainingStack ->
      SavepointCreated savepoint (traces <> newTraces) remainingStack

{- |
  INTERNAL: Releases a savepoint (and any savepoints that were created after
  it) from the stack, effectively "committing" it within the context of any
  outer transaction that may exist.

  If no matching savepoint can be found a 'Left' is produced with an error
  message.
-}
releaseTransactionStackSavepoint ::
  O.Savepoint ->
  TransactionStack trace ->
  Either String (TransactionStack trace)
releaseTransactionStackSavepoint targetSavepoint =
  let go ::
        DList.DList trace ->
        TransactionStack trace ->
        Either String (TransactionStack trace)
      go savedTraces stack =
        case stack of
          TransactionOpened _ ->
            Left "Tried to release savepoint that could not be found in EntityTrace TransactionStack"
          SavepointCreated currentSavepoint traces remainingStack ->
            if currentSavepoint == targetSavepoint
              then Right (appendTracesAtTopOfStack (traces <> savedTraces) remainingStack)
              else go (traces <> savedTraces) remainingStack
   in go DList.empty

{- |
  INTERNAL: Rolls back the transaction stack to a savepoint, eliminating any
  traces or savepoints that have happened since the savepoint was begun. Note
  that this does _not_ eliminate the targe savepoint, but does abandon any
  traces that are stored at its level of the stack, resetting to a state as if
  the savepoint had just been created.

  If no matching savepoint can be found a 'Left' is produced with an error message.
-}
rollbackTransactionStackToSavepoint ::
  O.Savepoint ->
  TransactionStack trace ->
  Either String (TransactionStack trace)
rollbackTransactionStackToSavepoint targetSavepoint =
  let go :: TransactionStack trace -> Either String (TransactionStack trace)
      go stack =
        case stack of
          TransactionOpened _ ->
            Left "Tried to rollback savepoint that could not be found in EntityTrace TransactionStack"
          SavepointCreated currentSavepoint _uncommitedTraces remainingStack ->
            if currentSavepoint == targetSavepoint
              then Right (SavepointCreated currentSavepoint DList.empty remainingStack)
              else go remainingStack
   in go

{- |
  INTERNAL: A starting point for 'RecordedTraces' where nothing has been
  recorded and no transaction stack has been created. This state corresponds to
  when no transaction has yet been opened by the actions invoked by
  'runEntityTraceT'. This is not exposed so that 'runEntityTraceT' can ensure
  that the 'RecordedTraces' and the 'O.OrvilleState' are in sync at the
  beginning of the traced action.
-}
emptyTraceData :: RecordedTraces trace
emptyTraceData = RecordedTraces mempty Nothing

{- |
  INTERNAL: A voided version of 'atomicModifyIORef'' since it is not offered
  as part of 'IORef'
-}
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (\a -> (f a, ()))

{- |
  INTERNAL: Mutates the 'EntityTraceState' 'IORef' to add some traces to it.
-}
addTracesToRecord :: EntityTraceState trace -> [trace] -> IO ()
addTracesToRecord traceState traces =
  atomicModifyIORef'_ (_entityTraceStateRef traceState) $ \recorded ->
    case currentTransactionStack recorded of
      Just uncommitted ->
        recorded
          { currentTransactionStack =
              Just $ appendTracesAtTopOfStack (DList.fromList traces) uncommitted
          }
      Nothing ->
        recorded
          { committedTracesDList =
              committedTracesDList recorded <> DList.fromList traces
          }

{- |
  INTERNAL: Responds to an Orville 'O.BeginTransaction' callback by mutating
  the 'EntityTraceState' to begin a new transaction stack. The 'EntityTraceState'
  is always expected to be in sync with the transaction state kept by orville,
  so if there is already a transaction stack open this raises an error.
-}
beginTransaction :: EntityTraceState trace -> IO ()
beginTransaction traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just _ ->
          (recorded, Just beginTransactionStateError)
        Nothing ->
          ( recorded
              { currentTransactionStack = Just (TransactionOpened DList.empty)
              }
          , Nothing
          )

  traverse_ Ex.throwIO mbError

{- |
  INTERNAL: Responds to an Orville 'O.CommitTransaction' callback by mutating
  the 'EntityTraceState' to commit the current transaction stack. The 'EntityTraceState'
  is always expected to be in sync with the transaction state kept by orville,
  so if there is no transaction stack this raises an error.
-}
commitTransaction :: EntityTraceState trace -> IO ()
commitTransaction traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just stack ->
          ( recorded
              { currentTransactionStack = Nothing
              , committedTracesDList = committedTracesDList recorded <> allTransactionStackTraces stack
              }
          , Nothing
          )
        Nothing ->
          (recorded, Just commitTransactionStateError)

  traverse_ Ex.throwIO mbError

{- |
  INTERNAL: Responds to an Orville 'O.RollbackTransaction' callback by mutating
  the 'EntityTraceState' to abandon the current transaction stack. The
  'EntityTraceState' is always expected to be in sync with the transaction
  state kept by orville, so if there is no transaction stack this raises an
  error.
-}
rollbackTransaction :: EntityTraceState trace -> IO ()
rollbackTransaction traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just _ ->
          (recorded {currentTransactionStack = Nothing}, Nothing)
        Nothing ->
          (recorded, Just rollbackTransactionStateError)

  traverse_ Ex.throwIO mbError

{- |
  INTERNAL: Responds to an Orville 'O.NewSavepoint' callback by mutating the
  'EntityTraceState' to add a new savpoint to the top with the given
  identifier. The 'EntityTraceState' is always expected to be in sync with the
  transaction state kept by orville, so if there is no transaction stack this
  raises an error.
-}
newSavepoint :: O.Savepoint -> EntityTraceState trace -> IO ()
newSavepoint savepoint traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just stack ->
          ( recorded
              { currentTransactionStack = Just (SavepointCreated savepoint DList.empty stack)
              }
          , Nothing
          )
        Nothing ->
          (recorded, Just newSavepointTransactionStateError)

  traverse_ Ex.throwIO mbError

{- |
  INTERNAL: Responds to an Orville 'O.ReleaseSavepoint' callback by mutating
  the 'EntityTraceState' to released the savepoint with provided identifier.
  The 'EntityTraceState' is always expected to be in sync with the transaction
  state kept by orville, so if there is no transaction stack or no matching
  savepoint can be found this raises an error.
-}
releaseSavepoint :: O.Savepoint -> EntityTraceState trace -> IO ()
releaseSavepoint savepoint traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just uncommitted ->
          case releaseTransactionStackSavepoint savepoint uncommitted of
            Left _ ->
              (recorded, Just releaseSavepointNoMatchingSavepointError)
            Right newStack ->
              ( recorded
                  { currentTransactionStack = Just newStack
                  }
              , Nothing
              )
        Nothing ->
          (recorded, Just releaseSavepointNoTransactionError)

  traverse_ Ex.throwIO mbError

{- |
  INTERNAL: Responds to an Orville 'O.RollbackToSavepoint' callback by mutating
  the 'EntityTraceState' to roll back to the savepoint with provided
  identifier. The 'EntityTraceState' is always expected to be in sync with the
  transaction state kept by orville, so if there is no transaction stack or no
  matching savepoint can be found this raises an error.
-}
rollbackToSavepoint :: O.Savepoint -> EntityTraceState trace -> IO ()
rollbackToSavepoint savepoint traceState = do
  mbError <-
    atomicModifyIORef' (_entityTraceStateRef traceState) $ \recorded ->
      case currentTransactionStack recorded of
        Just uncommitted ->
          case rollbackTransactionStackToSavepoint savepoint uncommitted of
            Left _ ->
              (recorded, Just rollbackToSavepointNoMatchingSavepointError)
            Right newStack ->
              ( recorded
                  { currentTransactionStack = Just newStack
                  }
              , Nothing
              )
        Nothing ->
          (recorded, Just rollbackToSavepointNoTransactionError)

  traverse_ Ex.throwIO mbError

{- |
  'EntityTraceT' is simply a synonym for 'ReaderT' over an 'EntityTraceState'.
  The 'EntityTraceState' keeps track of what traces have been recorded,
  including keeping them in sync with any transaction or savepoint operations
  that are performed via the 'O.withTransaction' function. I.E. if a
  transaction or savepoint is rolled-back any traces that had been recorded by
  'insertEntityTraced', 'updateEntityTrace', or 'deletedEntityTraced' are also
  rolled-back.

@since 0.10.0.0
-}
type EntityTraceT trace =
  ReaderT (EntityTraceState trace)

-- | @since 0.10.0.0
instance O.MonadOrville m => MonadEntityTrace trace (ReaderT (EntityTraceState trace) m) where
  recordTraces =
    recordTracesInState ask

  liftOrvilleUntraced untraced =
    -- The 'untraced' argument must be explict here for GHC 9
    lift untraced

-- | @since 0.10.0.0
instance O.MonadOrville m => O.MonadOrville (ReaderT (EntityTraceState trace) m)

{- |
  INTERNAL: Mutates the 'EntityTraceState' that is returned by the given "ask"
  operation. We may decide to expose this in the future so that it's possible
  to build an instance of 'MonadEntityTrace' by tracking an 'EntityTraceState'
  in a user-provided 'ReaderT' (or other) context, but to do so we would need
  to provide a relatively safe API for instantiating the 'EntityTraceState' and
  'O.OrvilleState' together to ensure that the transaction callback is
  registered and the transaction state between the two is in sync.
-}
recordTracesInState ::
  MonadIO m =>
  m (EntityTraceState trace) ->
  [trace] ->
  m ()
recordTracesInState askTraceState traces = do
  recordedTraces <- askTraceState
  liftIO $ addTracesToRecord recordedTraces traces

{- |
  'runEntityTraceT' runs an Orville action that has tracing behavior and
  returns the traces that were committed. Note that there will never be any
  uncommitted traces at the end because any 'O.withTransaction' block must by
  contained within the action passed  to 'runEntityTraceT'. This function
  creates a new 'O.OrvilleState' and passes it to the run function that you
  provide in order to enforce this.

  Note that if an exception occurs in 'm' and is not caught within the action passed
  to 'runEntityTraceT', you will lose any traces that may have happened up to
  the point of the action, including those related to database operations that were
  successfully committed. If you wish to respond to those traces, you need to catch
  the exception inside the action given to 'runEntityTraceT' so that it can return
  the traces to you.

@since 0.10.0.0
-}
runEntityTraceT ::
  MonadIO n =>
  O.ErrorDetailLevel ->
  O.Pool O.Connection ->
  (O.OrvilleState -> m a -> n a) ->
  EntityTraceT trace m a ->
  n (a, [trace])
runEntityTraceT errorDetailLevel pool runM traceT = do
  ref <- liftIO $ EntityTraceState <$> newIORef emptyTraceData
  let mAction =
        runReaderT traceT ref

      orvilleState =
        O.addTransactionCallback
          (trackTransactions ref)
          (O.newOrvilleState errorDetailLevel pool)

  a <- runM orvilleState mAction
  traceData <- liftIO (readIORef $ _entityTraceStateRef ref)

  case currentTransactionStack traceData of
    Nothing ->
      pure (a, DList.toList $ committedTracesDList traceData)
    Just _ ->
      liftIO $ Ex.throwIO transactionStillOpenAtEndError

{- |
  INTERNAL: This function is registered as the transaction callback on the
  'O.OrvilleState' to keep the given 'EntityTraceState' transaction stack
  in sync with operations performed by 'O.withTransaction'.
-}
trackTransactions :: EntityTraceState trace -> O.TransactionEvent -> IO ()
trackTransactions recorded event =
  case event of
    O.BeginTransaction -> beginTransaction recorded
    O.CommitTransaction -> commitTransaction recorded
    O.RollbackTransaction -> rollbackTransaction recorded
    O.NewSavepoint savepoint -> newSavepoint savepoint recorded
    O.ReleaseSavepoint savepoint -> releaseSavepoint savepoint recorded
    O.RollbackToSavepoint savepoint -> rollbackToSavepoint savepoint recorded

{- |
  INTERNAL: This error is raised by the transaction callback functions if
  the 'TransactionStack' is not in the expected state. If this is ever raised
  it is either a bug in orville, or a user executing transaction sql directly
  rather than going throw 'O.withTransaction'.
-}
newtype EntityTraceTransactionStateError
  = EntityTraceTransactionStateError String
  deriving
    ( -- | @since 0.10.0.0
      Show
    )

beginTransactionStateError :: EntityTraceTransactionStateError
beginTransactionStateError =
  EntityTraceTransactionStateError
    "Got BeginTransaction callback, but the EntityTraceState was already tracking a transaction."

commitTransactionStateError :: EntityTraceTransactionStateError
commitTransactionStateError =
  EntityTraceTransactionStateError
    "Got CommitTransaction callback, but the EntityTraceState was not tracking a transaction."

rollbackTransactionStateError :: EntityTraceTransactionStateError
rollbackTransactionStateError =
  EntityTraceTransactionStateError
    "Got RollbackTransaction callback, but the EntityTraceState was not tracking a transaction."

newSavepointTransactionStateError :: EntityTraceTransactionStateError
newSavepointTransactionStateError =
  EntityTraceTransactionStateError
    "Got NewSavepoint callback, but the EntityTraceState was not tracking a transaction."

releaseSavepointNoTransactionError :: EntityTraceTransactionStateError
releaseSavepointNoTransactionError =
  EntityTraceTransactionStateError
    "Got ReleaseSavepoint callback, but the EntityTraceState was not tracking a transaction."

releaseSavepointNoMatchingSavepointError :: EntityTraceTransactionStateError
releaseSavepointNoMatchingSavepointError =
  EntityTraceTransactionStateError
    "Got ReleaseSavepoint callback, but the EntityTraceState did not have a matching savepoint."

rollbackToSavepointNoTransactionError :: EntityTraceTransactionStateError
rollbackToSavepointNoTransactionError =
  EntityTraceTransactionStateError
    "Got RollbackToSavepoint callback, but the EntityTraceState was not tracking a transaction."

rollbackToSavepointNoMatchingSavepointError :: EntityTraceTransactionStateError
rollbackToSavepointNoMatchingSavepointError =
  EntityTraceTransactionStateError
    "Got RollbackToSavepoint callback, but the EntityTraceState did not have a matching savepoint."

transactionStillOpenAtEndError :: EntityTraceTransactionStateError
transactionStillOpenAtEndError =
  EntityTraceTransactionStateError
    "Finished runEntityTraceT with a transactions still open in the EntityTraceState."

-- | @since 0.10.0.0
instance Ex.Exception EntityTraceTransactionStateError
