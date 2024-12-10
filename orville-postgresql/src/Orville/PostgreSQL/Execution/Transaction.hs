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
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Numeric.Natural (Natural)

import qualified Orville.PostgreSQL.Execution.Execute as Execute
import qualified Orville.PostgreSQL.Execution.QueryType as QueryType
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Bracket as Bracket
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Monad as Monad
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

      beginTransaction :: Monad.MonadOrville m => m ()
      beginTransaction = do
        liftIO $ do
          let
            openEvent = OrvilleState.openTransactionEvent transaction
          executeTransactionSql (transactionEventSql state openEvent)
          callback openEvent

      doAction () =
        Monad.localOrvilleState
          (OrvilleState.connectState innerConnectedState)
          action

      finishTransaction :: MonadIO m => () -> Bracket.BracketResult -> m ()
      finishTransaction () result =
        liftIO $
          case result of
            Bracket.BracketSuccess -> do
              let
                successEvent = OrvilleState.transactionSuccessEvent transaction
              executeTransactionSql (transactionEventSql state successEvent)
              callback successEvent
            Bracket.BracketError -> do
              let
                rollbackEvent = OrvilleState.rollbackTransactionEvent transaction
              executeTransactionSql (transactionEventSql state rollbackEvent)
              callback rollbackEvent

    Bracket.bracketWithResult beginTransaction finishTransaction doAction

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
      RawSql.toRawSql $ Expr.rollback
    OrvilleState.RollbackToSavepoint savepoint ->
      RawSql.toRawSql $ Expr.rollbackTo (savepointName savepoint)
    OrvilleState.CommitTransaction ->
      RawSql.toRawSql $ Expr.commit
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

{- |
  Information about the current transaction state of an action passed to 'withTransaction'.

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

{- |
  Returns 'Just' an 'InWithTransaction' value when called inside of the action passed to
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
