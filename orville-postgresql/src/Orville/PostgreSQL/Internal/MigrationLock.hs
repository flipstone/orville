{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.MigrationLock
  ( MigrationLockOptions (migrationLockId, maxLockAttempts, delayBetweenLockAttemptsMicros, lockDelayVariationMicros)
  , defaultLockOptions
  , MigrationLockId
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
import qualified System.Random as Rand

import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Internal.Bracket as Bracket
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | 'MigrationLockOptions' controls how Orville acquires its PostgreSQL advisory lock
  to ensure that only one process is attempting to make schema changes at a time.
  See the descriptions of each of the options for more detail:

  * 'migrationLockId'
  * 'maxLockAttempts'
  * 'delayBetweenLockAttemptsMicros'
  * 'lockDelayVariationMicros'

@since 1.1.0.0
-}
data MigrationLockOptions = MigrationLockOptions
  { migrationLockId :: MigrationLockId
  {- ^ Specifies the 'MigrationLockId' for the lock that will be acquired to ensure
  only one process is attempting to migrate the schema at a time.

  @since 1.1.0.0
  -}
  , maxLockAttempts :: Int
  {- ^ The maximum number of times Orville will attempt to acquire the migration
  lock. If the lock has not bene acquired after this many attempts, a
  'MigrationLockError' will be thrown as an exception.

  @since 1.1.0.0
  -}
  , delayBetweenLockAttemptsMicros :: Int
  {- ^ The minimum number of microseconds Orville will wait after a failed attempt to
  acquire the lock before it tries again. A random variation will be added to this
  to determine the delay after each failed attempt. See 'lockDelayVariationMicros'

  @since 1.1.0.0
  -}
  , lockDelayVariationMicros :: Int
  {- ^ The maximum variation to add to the delay before attempting to acquire the lock
  again. The actual variation at each delay will be a pseudo-random number between
  @0@ and 'lockDelayVariationMicros'.

  @since 1.1.0.0
  -}
  }

{- | The default lock options use the 'defaultLockId', 25 attempts and randomized delay
between 100 and 125 milliseconds.

@since 1.1.0.0
-}
defaultLockOptions :: MigrationLockOptions
defaultLockOptions =
  MigrationLockOptions
    { migrationLockId = defaultLockId
    , maxLockAttempts = 25
    , delayBetweenLockAttemptsMicros = 100000
    , lockDelayVariationMicros = 25000
    }

{- | Identifies a PostgreSQL advisory lock to to be aquired by the application. Use
'defaultLockId' to obtain the default value and 'nextLockId' to create custom
values if you need them.

@since 1.0.0.0
-}
data MigrationLockId = MigrationLockId
  { i_lockKey1 :: Int32
  , i_lockKey2 :: Int32
  }

{- | The lock id that Orville uses by default to ensure that just one copy of the
application is attempting to run migrations at a time.

@since 1.0.0.0
-}
defaultLockId :: MigrationLockId
defaultLockId =
  MigrationLockId
    { i_lockKey1 = orvilleLockScope
    , i_lockKey2 = 7995632
    }

{- | Increments the id of the given 'MigrationLockId', creating a new distinct lock
id. You can use this to create your own custom 'MigrationLockId' values as
necessary if you need to control migration runs in a custom manner.

@since 1.0.0.0
-}
nextLockId :: MigrationLockId -> MigrationLockId
nextLockId lockId =
  lockId
    { i_lockKey2 = 1 + i_lockKey2 lockId
    }

orvilleLockScope :: Int32
orvilleLockScope = 17772

{- | Executes an Orville action with a PostgreSQL advisory lock held that
  indicates to other Orville processes that a database migration is being done
  and no others should be performed concurrently.

@since 1.0.0.0
-}
withMigrationLock ::
  Monad.MonadOrville m =>
  MigrationLockOptions ->
  m a ->
  m a
withMigrationLock options action =
  Monad.withConnection_ $
    Bracket.bracketWithResult
      (accquireTransactionLock options)
      (\() _bracketResult -> releaseTransactionLock options)
      (\() -> action)

accquireTransactionLock ::
  forall m.
  Monad.MonadOrville m =>
  MigrationLockOptions ->
  m ()
accquireTransactionLock options =
  let
    lockId =
      migrationLockId options

    maxAttempts =
      maxLockAttempts options

    delayMicros =
      delayBetweenLockAttemptsMicros options

    delayVariationMicros =
      lockDelayVariationMicros options

    go :: Int -> m ()
    go attempts = do
      locked <- attemptLockAcquisition
      if locked
        then pure ()
        else do
          MIO.liftIO $ do
            Monad.when (attempts >= maxAttempts) $ do
              throwIO $
                MigrationLockError
                  ("Giving up after " <> show maxAttempts <> " attempts to aquire the migration lock.")

            variation <- Rand.randomRIO (0, delayVariationMicros)
            threadDelay (delayMicros + variation)

          go $ attempts + 1

    attemptLockAcquisition :: m Bool
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

releaseTransactionLock :: Monad.MonadOrville m => MigrationLockOptions -> m ()
releaseTransactionLock =
  Exec.executeVoid Exec.OtherQuery . releaseLockExpr . migrationLockId

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

{- | Raised if 'withMigrationLock' cannot acquire the migration lock in a
  timely manner.

@since 1.0.0.0
-}
newtype MigrationLockError
  = MigrationLockError String
  deriving
    ( -- | @since 1.0.0.0
      Show
    )

-- | @since 1.0.0.0
instance Exception MigrationLockError
