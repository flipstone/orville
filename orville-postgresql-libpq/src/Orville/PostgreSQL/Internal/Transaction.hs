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
    let doAction = do
          liftIO $ RawSql.executeVoid conn (openTransactionSql transaction)
          value <-
            OrvilleState.localOrvilleState
              (OrvilleState.connectState innerConnectedState)
              action
          liftIO $ do
            RawSql.executeVoid conn (transactionSuccessSql transaction)
            liftIO $ IORef.writeIORef committed True
          pure value
    let rollbackUncommitted =
          liftIO $ do
            finished <- liftIO $ IORef.readIORef committed
            Monad.when (not finished) (RawSql.executeVoid conn $ rollbackTransactionSql transaction)
    MonadOrville.liftFinally Exception.finally doAction rollbackUncommitted

openTransactionSql :: OrvilleState.TransactionState -> RawSql.RawSql
openTransactionSql txnState =
  case txnState of
    OrvilleState.OutermostTransaction ->
      RawSql.toRawSql $ Expr.beginTransaction Nothing
    OrvilleState.SavepointTransaction savepoint ->
      RawSql.toRawSql $ Expr.savepoint (savepointName savepoint)

rollbackTransactionSql :: OrvilleState.TransactionState -> RawSql.RawSql
rollbackTransactionSql txnState =
  case txnState of
    OrvilleState.OutermostTransaction ->
      RawSql.toRawSql $ Expr.rollback
    OrvilleState.SavepointTransaction savepoint ->
      RawSql.toRawSql $ Expr.rollbackTo (savepointName savepoint)

transactionSuccessSql :: OrvilleState.TransactionState -> RawSql.RawSql
transactionSuccessSql txnState =
  case txnState of
    OrvilleState.OutermostTransaction ->
      RawSql.toRawSql $ Expr.commit
    OrvilleState.SavepointTransaction savepoint ->
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
