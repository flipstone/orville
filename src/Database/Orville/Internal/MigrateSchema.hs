{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Internal.MigrateSchema
  ( migrateSchema
  , MigrationError(..)
  ) where

import            Control.Concurrent (threadDelay)
import            Control.Monad
import            Control.Monad.Catch
import            Control.Monad.IO.Class
import            Data.Convertible
import            Data.Data
import            Data.Int
import            Data.Monoid
import            Data.String
import            Data.Typeable
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.Expr
import            Database.Orville.Internal.FromClause
import            Database.Orville.Internal.FromSql
import            Database.Orville.Internal.MigrateConstraint
import            Database.Orville.Internal.MigrateIndex
import            Database.Orville.Internal.MigrateTable
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Select
import            Database.Orville.Internal.Types
import            Database.Orville.Select
import            Database.Orville.Raw

orvilleLockScope :: Int32
orvilleLockScope = 17772

migrationLockId :: Int32
migrationLockId = 7995632

tryLockExpr :: SelectExpr
tryLockExpr = rawSqlExpr $ "pg_try_advisory_xact_lock("
                        <> fromString (show orvilleLockScope)
                        <> ","
                        <> fromString (show migrationLockId)
                        <> ") as result"

waitForLockExpr :: SelectExpr
waitForLockExpr = rawSqlExpr $ "pg_advisory_xact_lock("
                             <> fromString (show orvilleLockScope)
                             <> ","
                             <> fromString (show migrationLockId)
                             <> ") as result"

lockResult :: FromSql Bool
lockResult = col ("result" :: String)

withLockedTransaction :: (MonadOrville conn m, MonadThrow m) => m a -> m a
withLockedTransaction action = do
    go 0
  where
    go attempts = do
      result <- runWithTransaction

      case result of
        Just a ->
          pure a

        Nothing -> do
          when (attempts >= 25) $ do
            throwM $ MigrationLockExcessiveRetryError
              "Giving up after 25 attempts to aquire the migration lock."

          liftIO $ threadDelay 10000
          go $ attempts + 1

    runWithTransaction =
      withTransaction $ do
        [locked] <- runSelect $ selectQueryColumns [tryLockExpr]
                                                   lockResult
                                                   (fromClauseRaw "")
                                                   mempty

        if locked
           then Just <$> action
           else do
             void $ runSelect $ selectQueryColumns [waitForLockExpr]
                                                   (pure ())
                                                   (fromClauseRaw "")
                                                   mempty
             pure Nothing

migrateSchema :: SchemaDefinition -> Orville ()
migrateSchema schemaDef =
  withConnection $ \conn -> do
    withLockedTransaction $ do
      tables <- liftIO $ getTables conn
      indexes <- liftIO $ getIndexes conn
      constraints <- liftIO $ getConstraints conn

      forM_ schemaDef $ \table ->
        case table of
        Table tableDef ->
          if tableName tableDef `elem` tables
          then migrateTable conn tableDef
          else createTable conn tableDef

        DropTable name ->
          when (name `elem` tables)
               (dropTable conn name)

        Index indexDef ->
          when (not $ indexName indexDef `elem` indexes)
               (createIndex conn indexDef)

        DropIndex name ->
          when (name `elem` indexes)
               (dropIndex conn name)

        Constraint constraintDef ->
          when (not $ constraintName constraintDef `elem` constraints)
               (createConstraint conn constraintDef)

        DropConstraint tablName name ->
          when (name `elem` constraints)
               (dropConstraint conn tablName name)

data MigrationError =
    MigrationLockExcessiveRetryError String
  deriving (Data, Typeable, Show)

instance Exception MigrationError

