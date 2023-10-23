{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.MigrationLock
  ( MigrationLockId
  , defaultLockId
  , nextLockId
  , withMigrationLock
  , MigrationLockError
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import Data.Int (Int32)

import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Internal.Bracket as Bracket
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
Identifies a PostgreSQL advisory lock to to be aquired by the application. Use
'defaultLockId' to obtain the default value and 'nextLockId' to create custom
values if you need them.

@since 1.0.0.0
-}
data MigrationLockId = MigrationLockId
  { i_lockKey1 :: Int32
  , i_lockKey2 :: Int32
  }

{- |
The lock id that Orville uses by default to ensure that just one copy of the
application is attempting to run migrations at a time.

@since 1.0.0.0
-}
defaultLockId :: MigrationLockId
defaultLockId =
  MigrationLockId
    { i_lockKey1 = orvilleLockScope
    , i_lockKey2 = 7995632
    }

{- |
Increments the 'lockKey2' of the given 'MigrationLockId', creating a new
distinct lock id. You can use this to create your own custom 'MigrationLockId'
values as necessary if you need to control migration runs in a custom manner.

@since 1.0.0.0
-}
nextLockId :: MigrationLockId -> MigrationLockId
nextLockId lockId =
  lockId
    { i_lockKey2 = 1 + i_lockKey2 lockId
    }

orvilleLockScope :: Int32
orvilleLockScope = 17772

{- |
  Executes an Orville action with a PostgreSQL advisory lock held that
  indicates to other Orville processes that a database migration is being done
  and no others should be performed concurrently.

@since 1.0.0.0
-}
withMigrationLock ::
  Monad.MonadOrville m =>
  MigrationLockId ->
  m a ->
  m a
withMigrationLock lockId action =
  Monad.withConnection_ $
    Bracket.bracketWithResult
      (accquireTransactionLock lockId)
      (\() _bracketResult -> releaseTransactionLock lockId)
      (\() -> action)

accquireTransactionLock ::
  forall m.
  Monad.MonadOrville m =>
  MigrationLockId ->
  m ()
accquireTransactionLock lockId =
  let
    go :: Int -> m ()
    go attempts = do
      locked <- attemptLockAcquisition
      if locked
        then pure ()
        else do
          MIO.liftIO $ do
            Monad.when (attempts >= 25) $ do
              throwIO $
                MigrationLockError
                  "Giving up after 25 attempts to aquire the migration lock."
            threadDelay 10000

          go $ attempts + 1

    attemptLockAcquisition = do
      tryLockResults <-
        Exec.executeAndDecode Exec.OtherQuery (tryLockExpr lockId) lockedMarshaller

      case tryLockResults of
        [locked] ->
          pure locked
        rows ->
          MIO.liftIO . throwIO . MigrationLockError $
            "Expected exactly one row from attempt to acquire migration lock, but got " <> show (length rows)
  in
    go 0

releaseTransactionLock :: Monad.MonadOrville m => MigrationLockId -> m ()
releaseTransactionLock =
  Exec.executeVoid Exec.OtherQuery . releaseLockExpr

lockedMarshaller :: Marshall.AnnotatedSqlMarshaller Bool Bool
lockedMarshaller =
  Marshall.annotateSqlMarshallerEmptyAnnotation $
    Marshall.marshallField id (Marshall.booleanField "locked")

tryLockExpr :: MigrationLockId -> RawSql.RawSql
tryLockExpr lockId =
  RawSql.fromString "SELECT pg_try_advisory_lock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 (i_lockKey1 lockId))
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 (i_lockKey2 lockId))
    <> RawSql.rightParen
    <> RawSql.fromString " as locked"

releaseLockExpr :: MigrationLockId -> RawSql.RawSql
releaseLockExpr lockId =
  RawSql.fromString "SELECT pg_advisory_unlock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 (i_lockKey1 lockId))
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 (i_lockKey2 lockId))
    <> RawSql.rightParen

{- |
  Raised if 'withMigrationLock' cannot acquire the migration lock in a
  timely manner.

@since 1.0.0.0
-}
newtype MigrationLockError
  = MigrationLockError String
  deriving (Show)

instance Exception MigrationLockError
