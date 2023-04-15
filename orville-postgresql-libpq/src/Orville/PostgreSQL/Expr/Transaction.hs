{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Transaction
  ( BeginTransactionExpr,
    beginTransaction,
    TransactionMode,
    readWrite,
    readOnly,
    deferrable,
    notDeferrable,
    isolationLevel,
    IsolationLevel,
    serializable,
    repeatableRead,
    readCommitted,
    readUncommitted,
    CommitExpr,
    commit,
    RollbackExpr,
    rollback,
    rollbackTo,
    SavepointExpr,
    savepoint,
    ReleaseSavepointExpr,
    releaseSavepoint,
  )
where

import Data.Maybe (maybeToList)

import qualified Orville.PostgreSQL.Expr.Name as Name
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype BeginTransactionExpr
  = BeginTransactionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

beginTransaction :: Maybe TransactionMode -> BeginTransactionExpr
beginTransaction maybeTransactionMode =
  BeginTransactionExpr $
    RawSql.intercalate RawSql.space $
      ( RawSql.fromString "BEGIN TRANSACTION" :
        maybeToList (RawSql.toRawSql <$> maybeTransactionMode)
      )

newtype TransactionMode
  = TransactionMode RawSql.RawSql
  deriving (RawSql.SqlExpression)

readWrite :: TransactionMode
readWrite =
  TransactionMode (RawSql.fromString "READ WRITE")

readOnly :: TransactionMode
readOnly =
  TransactionMode (RawSql.fromString "READ ONLY")

deferrable :: TransactionMode
deferrable =
  TransactionMode (RawSql.fromString "DEFERRABLE")

notDeferrable :: TransactionMode
notDeferrable =
  TransactionMode (RawSql.fromString "NOT DEFERRABLE")

isolationLevel :: IsolationLevel -> TransactionMode
isolationLevel level =
  TransactionMode $
    (RawSql.fromString "ISOLATION LEVEL " <> RawSql.toRawSql level)

newtype IsolationLevel
  = IsolationLevel RawSql.RawSql
  deriving (RawSql.SqlExpression)

serializable :: IsolationLevel
serializable =
  IsolationLevel (RawSql.fromString "SERIALIZABLE")

repeatableRead :: IsolationLevel
repeatableRead =
  IsolationLevel (RawSql.fromString "REPEATABLE READ")

readCommitted :: IsolationLevel
readCommitted =
  IsolationLevel (RawSql.fromString "READ COMMITTED")

readUncommitted :: IsolationLevel
readUncommitted =
  IsolationLevel (RawSql.fromString "READ UNCOMMITTED")

newtype CommitExpr
  = CommitExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

commit :: CommitExpr
commit =
  CommitExpr (RawSql.fromString "COMMIT")

newtype RollbackExpr
  = RollbackExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

rollback :: RollbackExpr
rollback =
  RollbackExpr (RawSql.fromString "ROLLBACK")

rollbackTo :: Name.SavepointName -> RollbackExpr
rollbackTo savepointName =
  RollbackExpr $
    RawSql.fromString "ROLLBACK TO SAVEPOINT " <> RawSql.toRawSql savepointName

newtype SavepointExpr
  = SavepointExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

savepoint :: Name.SavepointName -> SavepointExpr
savepoint savepointName =
  SavepointExpr $
    RawSql.fromString "SAVEPOINT " <> RawSql.toRawSql savepointName

newtype ReleaseSavepointExpr
  = ReleaseSavepontExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

releaseSavepoint :: Name.SavepointName -> ReleaseSavepointExpr
releaseSavepoint savepointName =
  ReleaseSavepontExpr $
    RawSql.fromString "RELEASE SAVEPOINT " <> RawSql.toRawSql savepointName
