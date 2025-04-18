{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

This module provides the functionality to work with SQL transactions - notably
to ensure some Haskell action occurs within a database transaction.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.Transaction
  ( withTransaction
  , inWithTransaction
  , InWithTransaction (InOutermostTransaction, InSavepointTransaction)
  , UnexpectedTransactionStatusError (..)
  )
where

import Control.Exception (Exception, finally, onException, throwIO, try, uninterruptibleMask)
import Control.Monad.IO.Class (liftIO)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Numeric.Natural (Natural)

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Bracket as Bracket
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Raw.Connection as Connection
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Performs an action in an Orville monad within a database transaction. The transaction
  is begun before the action is called. If the action completes without raising an exception,
  the transaction will be committed. If the action raises an exception, the transaction will
  rollback.

  This function is safe to call from within another transaction. When called this way, the
  transaction will establish a new savepoint at the beginning of the nested transaction and
  either release the savepoint or rollback to it as appropriate.

  Note: Exceptions are handled using the implementations of 'Monad.catch' and
  'Monad.mask' provided by the 'Monad.MonadOrvilleControl' instance for @m@.

@since 1.0.0.0
-}
withTransaction :: Monad.MonadOrville m => m a -> m a
withTransaction action =
  MonadOrville.withConnectedState $ \connectedState -> do
    let
      conn = OrvilleState.connectedConnection connectedState
      transaction = OrvilleState.newTransaction (OrvilleState.connectedTransaction connectedState)

      innerConnectedState =
        connectedState
          { OrvilleState.connectedTransaction = Just transaction
          }

    state <- Monad.askOrvilleState

    let
      executeTransactionSql :: RawSql.RawSql -> IO ()
      executeTransactionSql sql =
        Execute.executeVoidIO QueryType.OtherQuery sql state conn

      callback =
        OrvilleState.orvilleTransactionCallback state

      beginTransaction :: IO ()
      beginTransaction = do
        status <- Connection.transactionStatusOrThrow conn
        let
          openEvent = OrvilleState.openTransactionEvent transaction
          beginAction = do
            executeTransactionSql (transactionEventSql state openEvent)
            callback openEvent
          transactionError = UnexpectedTransactionStatusError status openEvent
        case status of
          LibPQ.TransIdle -> case openEvent of
            OrvilleState.BeginTransaction -> do
              let
                execRollback :: (IO () -> IO a) -> IO a
                execRollback restore = do
                  executeTransactionSql (transactionEventSql state OrvilleState.RollbackTransaction)
                  restore $ callback OrvilleState.RollbackTransaction
                rollback = uninterruptibleMask $ \restore -> do
                  postBeginStatus <- Connection.transactionStatusOrThrow conn
                  case postBeginStatus of
                    LibPQ.TransInTrans -> execRollback restore
                    LibPQ.TransInError -> execRollback restore
                    LibPQ.TransActive -> execRollback restore
                    LibPQ.TransIdle -> pure ()
                    LibPQ.TransUnknown -> pure ()
              beginAction `onException` rollback
            _ ->
              throwIO transactionError
          LibPQ.TransInTrans -> case openEvent of
            OrvilleState.NewSavepoint _ ->
              beginAction
            _ ->
              throwIO transactionError
          LibPQ.TransActive ->
            throwIO transactionError
          LibPQ.TransInError ->
            throwIO transactionError
          LibPQ.TransUnknown ->
            throwIO transactionError

      doAction () =
        Monad.localOrvilleState
          (OrvilleState.connectState innerConnectedState)
          action

      finishTransaction :: Bracket.BracketResult -> IO ()
      finishTransaction result = uninterruptibleMask $ \restore -> do
        status <- Connection.transactionStatusOrThrow conn
        let
          successEvent = OrvilleState.transactionSuccessEvent transaction
          rollbackEvent = OrvilleState.rollbackTransactionEvent transaction
          rollback = do
            executeTransactionSql (transactionEventSql state rollbackEvent)
            restore $ callback rollbackEvent
          transactionError = UnexpectedTransactionStatusError status $ case result of
            Bracket.BracketSuccess -> successEvent
            Bracket.BracketError -> rollbackEvent
        case status of
          LibPQ.TransInTrans -> case result of
            Bracket.BracketSuccess -> do
              eSuccess <- try $ executeTransactionSql (transactionEventSql state successEvent)
              case eSuccess of
                Right () ->
                  restore $ callback successEvent
                Left ex -> do
                  restore (callback rollbackEvent) `finally` throwIO (ex :: Connection.SqlExecutionError)
            Bracket.BracketError ->
              rollback
          LibPQ.TransInError ->
            rollback
          LibPQ.TransActive ->
            throwIO transactionError
          LibPQ.TransIdle ->
            throwIO transactionError
          LibPQ.TransUnknown ->
            throwIO transactionError

    Bracket.bracketWithResult
      (liftIO beginTransaction)
      (const $ liftIO . finishTransaction)
      doAction

{- | 'withTransaction' will throw this exception if libpq reports a transaction status on the underlying
  connection that is incompatible with the current transaction event.

@since 1.1.0.0
-}
data UnexpectedTransactionStatusError = UnexpectedTransactionStatusError
  { unexpectedTransactionStatusErrorTransactionStatus :: LibPQ.TransactionStatus
  , unexpectedTransactionStatusErrorTransactionEvent :: OrvilleState.TransactionEvent
  }

{- |

@since 1.1.0.0
-}
instance Show UnexpectedTransactionStatusError where
  show (UnexpectedTransactionStatusError status event) =
    "Unexpected transaction status during event " <> show event <> ": " <> show status

{- |

@since 1.1.0.0
-}
instance Exception UnexpectedTransactionStatusError

transactionEventSql ::
  OrvilleState.OrvilleState ->
  OrvilleState.TransactionEvent ->
  RawSql.RawSql
transactionEventSql state event =
  case event of
    OrvilleState.BeginTransaction ->
      RawSql.toRawSql $ OrvilleState.orvilleBeginTransactionExpr state
    OrvilleState.NewSavepoint savepoint ->
      RawSql.toRawSql $ Expr.savepoint (savepointName savepoint)
    OrvilleState.RollbackTransaction ->
      RawSql.toRawSql Expr.rollback
    OrvilleState.RollbackToSavepoint savepoint ->
      RawSql.toRawSql $ Expr.rollbackTo (savepointName savepoint)
    OrvilleState.CommitTransaction ->
      RawSql.toRawSql Expr.commit
    OrvilleState.ReleaseSavepoint savepoint ->
      RawSql.toRawSql $ Expr.releaseSavepoint (savepointName savepoint)

{- | INTERNAL: Constructs a savepoint name based on the current nesting level of
  transactions, as tracked by the `OrvilleState.Savepoint` type. Strictly
  speaking this is not necessary for PostgreSQL because it supports shadowing
  savepoint names. The SQL standard doesn't allow for savepoint name shadowing,
  however. Re-using this same name in other databases would overwrite the
  savepoint rather than shadow it. This function constructs savepoint names
  that will work on any database that implements savepoints accordings to the
  SQL standard even though Orville only supports PostgreSQL currently.

@since 1.0.0.0
-}
savepointName :: OrvilleState.Savepoint -> Expr.SavepointName
savepointName savepoint =
  let
    n = OrvilleState.savepointNestingLevel savepoint
  in
    Expr.savepointName ("orville_savepoint_level_" <> show n)

{- | Information about the current transaction state of an action passed to 'withTransaction'.

@since 1.1.0.0
-}
data InWithTransaction
  = InOutermostTransaction
  | -- | The 'Natural' indicates the savepoint depth, where @1@ is the first savepoint.
    InSavepointTransaction Natural
  deriving
    ( -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    , -- | @since 1.1.0.0
      Show
    )

{- |  Returns 'Just' an 'InWithTransaction' value when called inside of the action passed to
  'withTransaction', and 'Nothing' otherwise.

@since 1.1.0.0
-}
inWithTransaction :: MonadOrville.MonadOrville m => m (Maybe InWithTransaction)
inWithTransaction =
  fmap
    ( \state -> case OrvilleState.orvilleConnectionState state of
        OrvilleState.Connected connectedState ->
          fmap
            ( \transactionState -> case transactionState of
                OrvilleState.OutermostTransaction ->
                  InOutermostTransaction
                OrvilleState.SavepointTransaction i ->
                  InSavepointTransaction . fromIntegral $ OrvilleState.savepointNestingLevel i
            )
            (OrvilleState.connectedTransaction connectedState)
        OrvilleState.NotConnected ->
          Nothing
    )
    Monad.askOrvilleState
