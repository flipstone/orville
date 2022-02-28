{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Orville.PostgreSQL.Internal.Transaction
  ( withTransaction,
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.IORef as IORef

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.OrvilleState as OrvilleState
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

{- |
  Performs a an action in an Orville monad within a database transaction. The transaction
  in begun before the action is called. If the action completes without raising an exception,
  the transaction will be comitted. If the action raises an exception, the transaction will
  rollback.

  This function in save to call from within another transaction. When called this way the
  transaction will establish a new savepoint at the beginning of the nested transaction and
  either release the savepoint or rollback to it as appropriate.

  Note: Exceptions are handled using the implementation of
  'MonadOrville.liftFinally' provided by the 'MonadOrville' instance for @m@.
-}
withTransaction :: MonadOrville.MonadOrville m => m a -> m a
withTransaction action =
  MonadOrville.withConnectedState $ \connectedState -> do
    let conn = OrvilleState.connectedConnection connectedState
        transaction = OrvilleState.newTransaction (OrvilleState.connectedTransaction connectedState)

        innerConnectedState =
          connectedState
            { OrvilleState.connectedTransaction = Just transaction
            }

    committed <- liftIO $ IORef.newIORef False
    callback <- OrvilleState.orvilleTransactionCallback <$> OrvilleState.askOrvilleState

    let doAction = do
          liftIO $ do
            let openEvent = OrvilleState.openTransactionEvent transaction
            RawSql.executeVoid conn (transactionEventSql openEvent)
            callback openEvent

          value <-
            OrvilleState.localOrvilleState
              (OrvilleState.connectState innerConnectedState)
              action
          liftIO $ do
            let successEvent = OrvilleState.transactionSuccessEvent transaction
            RawSql.executeVoid conn (transactionEventSql successEvent)
            liftIO $ IORef.writeIORef committed True
            callback successEvent
          pure value
    let rollbackUncommitted =
          liftIO $ do
            finished <- IORef.readIORef committed
            Monad.when (not finished) $ do
              let rollbackEvent = OrvilleState.rollbackTransactionEvent transaction
              RawSql.executeVoid conn (transactionEventSql rollbackEvent)
              callback rollbackEvent

    MonadOrville.liftFinally Exception.finally doAction rollbackUncommitted

transactionEventSql :: OrvilleState.TransactionEvent -> RawSql.RawSql
transactionEventSql event =
  case event of
    OrvilleState.BeginTransaction ->
      RawSql.toRawSql $ Expr.beginTransaction Nothing
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

{- |
  INTERNAL: Constructs a savepoint name based on the current nesting level of
  transactions, as tracked by the `OrvilleState.Savepoint` type. Strictly
  speaking this is not necessary for PostgreSQL because it supports shadowing
  savepoint names. The SQL standard doesn't allow for savepoint name shadowing,
  however. Re-using this same name in other databases would overwrite the
  savepoint rather than shadow it. This function constructs savepoint names
  that will work on any database that implements savepoints accordings to the
  SQL standard even though Orville only supports PostgreSQL currently.
-}
savepointName :: OrvilleState.Savepoint -> Expr.SavepointName
savepointName savepoint =
  let n = OrvilleState.savepointNestingLevel savepoint
   in Expr.savepointName ("orville_savepoint_level_" <> show n)
