{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.Transaction
  ( BeginTransactionExpr
  , beginTransaction
  , TransactionMode
  , readWrite
  , readOnly
  , deferrable
  , notDeferrable
  , isolationLevel
  , IsolationLevel
  , serializable
  , repeatableRead
  , readCommitted
  , readUncommitted
  , CommitExpr
  , commit
  , RollbackExpr
  , rollback
  , rollbackTo
  , SavepointExpr
  , savepoint
  , ReleaseSavepointExpr
  , releaseSavepoint
  )
where

import Data.Maybe (maybeToList)

import qualified Orville.PostgreSQL.Expr.Name as Name
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent the name of a begin transaction statement. E.G.

> BEGIN TRANSACTION

'BeginTransactionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype BeginTransactionExpr
  = BeginTransactionExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'BeginTransactionExpr' that will begin a transaction using
  the specified mode, if any.

  @since 0.10.0.0
-}
beginTransaction :: Maybe TransactionMode -> BeginTransactionExpr
beginTransaction maybeTransactionMode =
  BeginTransactionExpr $
    RawSql.intercalate RawSql.space $
      ( RawSql.fromString "BEGIN TRANSACTION"
          : maybeToList (RawSql.toRawSql <$> maybeTransactionMode)
      )

{- |
Type to represent the a transaction mode. E.G.

> ISOLATION LEVEL SERIALIZABLE

'TransactionMode' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype TransactionMode
  = TransactionMode RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  The @READ WRITE@ transaction mode.

  @since 0.10.0.0
-}
readWrite :: TransactionMode
readWrite =
  TransactionMode (RawSql.fromString "READ WRITE")

{- |
  The @READ ONLY@ transaction mode.

  @since 0.10.0.0
-}
readOnly :: TransactionMode
readOnly =
  TransactionMode (RawSql.fromString "READ ONLY")

{- |
  The @DEFERRABLE@ transaction mode.

  @since 0.10.0.0
-}
deferrable :: TransactionMode
deferrable =
  TransactionMode (RawSql.fromString "DEFERRABLE")

{- |
  The @NOT DEFERRABLE@ transaction mode.

  @since 0.10.0.0
-}
notDeferrable :: TransactionMode
notDeferrable =
  TransactionMode (RawSql.fromString "NOT DEFERRABLE")

{- |
  An @ISOLATION LEVEL@ transaction mode with the given 'IsolationLevel'.

  @since 0.10.0.0
-}
isolationLevel :: IsolationLevel -> TransactionMode
isolationLevel level =
  TransactionMode $
    (RawSql.fromString "ISOLATION LEVEL " <> RawSql.toRawSql level)

{- |
Type to represent the a transaction isolation level. E.G.

> SERIALIZABLE

'IsolationLevel' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype IsolationLevel
  = IsolationLevel RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  The @SERIALIZABLE@ isolation level.

  @since 0.10.0.0
-}
serializable :: IsolationLevel
serializable =
  IsolationLevel (RawSql.fromString "SERIALIZABLE")

{- |
  The @REPEATABLE READ@ isolation level.

  @since 0.10.0.0
-}
repeatableRead :: IsolationLevel
repeatableRead =
  IsolationLevel (RawSql.fromString "REPEATABLE READ")

{- |
  The @READ COMMITTED@ isolation level.

  @since 0.10.0.0
-}
readCommitted :: IsolationLevel
readCommitted =
  IsolationLevel (RawSql.fromString "READ COMMITTED")

{- |
  The @READ UNCOMMITTED@ isolation level.

  @since 0.10.0.0
-}
readUncommitted :: IsolationLevel
readUncommitted =
  IsolationLevel (RawSql.fromString "READ UNCOMMITTED")

{- |
Type to represent the transaction commit statement. E.G.

> COMMIT

'CommitExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype CommitExpr
  = CommitExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A @COMMIT@ transaction statement

  @since 0.10.0.0
-}
commit :: CommitExpr
commit =
  CommitExpr (RawSql.fromString "COMMIT")

{- |
Type to represent the transaction rollback statement. E.G.

> ROLLBACK

'RollbackExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype RollbackExpr
  = RollbackExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A @ROLLBACK@ transaction statement

  @since 0.10.0.0
-}
rollback :: RollbackExpr
rollback =
  RollbackExpr (RawSql.fromString "ROLLBACK")

{- |
  A @ROLLBACK TO@ transaction statement that will rollback to the specified
  savepoint.

  @since 0.10.0.0
-}
rollbackTo :: Name.SavepointName -> RollbackExpr
rollbackTo savepointName =
  RollbackExpr $
    RawSql.fromString "ROLLBACK TO SAVEPOINT " <> RawSql.toRawSql savepointName

{- |
Type to represent the transaction savepoint statement. E.G.

> SAVEPOINT foo

'SavepointExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype SavepointExpr
  = SavepointExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A @SAVEPOINT@ statement that will create a savepoint with the given name.

  @since 0.10.0.0
-}
savepoint :: Name.SavepointName -> SavepointExpr
savepoint savepointName =
  SavepointExpr $
    RawSql.fromString "SAVEPOINT " <> RawSql.toRawSql savepointName

{- |
Type to represent the transaction release savepoint statement. E.G.

> RELEASE SAVEPOINT foo

'ReleaseSavepointExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype ReleaseSavepointExpr
  = ReleaseSavepontExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  A @RELEASE SAVEPOINT@ statement that will release the specified savepoint.

  @since 0.10.0.0
-}
releaseSavepoint :: Name.SavepointName -> ReleaseSavepointExpr
releaseSavepoint savepointName =
  ReleaseSavepontExpr $
    RawSql.fromString "RELEASE SAVEPOINT " <> RawSql.toRawSql savepointName
