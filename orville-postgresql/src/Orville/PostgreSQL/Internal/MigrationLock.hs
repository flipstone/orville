{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.MigrationLock
  ( withMigrationLock
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
  Executes an Orville action with a PostgreSQL advisory lock held that
  indicates to other Orville processes that a database migration is being done
  an no others should be performed concurrently.

@since 1.0.0.0
-}
withMigrationLock :: Monad.MonadOrville m => m a -> m a
withMigrationLock action =
  Monad.withConnection_ $
    Bracket.bracketWithResult
      accquireTransactionLock
      (\() _bracketResult -> releaseTransactionLock)
      (\() -> action)

accquireTransactionLock :: forall m. Monad.MonadOrville m => m ()
accquireTransactionLock =
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
        Exec.executeAndDecode Exec.OtherQuery tryLockExpr lockedMarshaller

      case tryLockResults of
        [locked] ->
          pure locked
        rows ->
          MIO.liftIO . throwIO . MigrationLockError $
            "Expected exactly one row from attempt to acquire migration lock, but got " <> show (length rows)
  in
    go 0

releaseTransactionLock :: Monad.MonadOrville m => m ()
releaseTransactionLock =
  Exec.executeVoid Exec.OtherQuery releaseLockExpr

orvilleLockScope :: Int32
orvilleLockScope = 17772

migrationLockId :: Int32
migrationLockId = 7995632

lockedMarshaller :: Marshall.AnnotatedSqlMarshaller Bool Bool
lockedMarshaller =
  Marshall.annotateSqlMarshallerEmptyAnnotation $
    Marshall.marshallField id (Marshall.booleanField "locked")

tryLockExpr :: RawSql.RawSql
tryLockExpr =
  RawSql.fromString "SELECT pg_try_advisory_lock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 orvilleLockScope)
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 migrationLockId)
    <> RawSql.rightParen
    <> RawSql.fromString " as locked"

releaseLockExpr :: RawSql.RawSql
releaseLockExpr =
  RawSql.fromString "SELECT pg_advisory_unlock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 orvilleLockScope)
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 migrationLockId)
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
