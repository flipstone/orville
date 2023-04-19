{-# LANGUAGE ScopedTypeVariables #-}

module Orville.PostgreSQL.Internal.MigrationLock
  ( withLockedTransaction,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as MIO
import Data.Int (Int32)

import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

withLockedTransaction :: forall m a. Monad.MonadOrville m => m a -> m a
withLockedTransaction action = do
  go 0
  where
    go :: Int -> m a
    go attempts = do
      result <- runWithTransaction
      case result of
        Just a -> pure a
        Nothing -> do
          MIO.liftIO $ do
            Monad.when (attempts >= 25) $ do
              throwIO $
                MigrationLockError
                  "Giving up after 25 attempts to aquire the migration lock."
            threadDelay 10000

          go $ attempts + 1
    runWithTransaction =
      Exec.withTransaction $ do
        tryLockResults <-
          Exec.executeAndDecode Exec.OtherQuery tryLockExpr lockedMarshaller

        case tryLockResults of
          [True] ->
            -- If we were able to acquire the lock then we can go ahead and
            -- execute the action.
            Just <$> action
          [False] -> do
            -- If we were not able to acquire the lock, wait for the lock to
            -- become available. However, the state of the database schema may
            -- have changed while we were waiting (for instance, another
            -- Orville process migrating the same schema). We must exit the
            -- current transaction and enter a new one, acquiring the lock
            -- again in that new transaction.
            Exec.executeVoid Exec.OtherQuery waitForLockExpr
            pure Nothing
          rows ->
            MIO.liftIO . throwIO . MigrationLockError $
              "Expected exactly one row from attempt to acquire migration lock, but got " <> show (length rows)

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
  RawSql.fromString "SELECT pg_try_advisory_xact_lock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 orvilleLockScope)
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 migrationLockId)
    <> RawSql.rightParen
    <> RawSql.fromString " as locked"

waitForLockExpr :: RawSql.RawSql
waitForLockExpr =
  RawSql.fromString "SELECT pg_advisory_xact_lock"
    <> RawSql.leftParen
    <> RawSql.parameter (SqlValue.fromInt32 orvilleLockScope)
    <> RawSql.comma
    <> RawSql.parameter (SqlValue.fromInt32 migrationLockId)
    <> RawSql.rightParen

newtype MigrationLockError
  = MigrationLockError String
  deriving (Show)

instance Exception MigrationLockError
