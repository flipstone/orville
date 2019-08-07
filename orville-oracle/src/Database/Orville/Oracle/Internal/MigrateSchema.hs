{-|
Module    : Database.Orville.Oracle.Internal.MigrateSchema
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Orville.Oracle.Internal.MigrateSchema
  ( migrateSchema
  , generateMigrationPlan
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.String
import Database.HDBC hiding (withTransaction)

import Database.Orville.Oracle.Internal.MappendCompat ((<>))

import Database.Orville.Oracle.Internal.Execute
import Database.Orville.Oracle.Internal.Expr
import Database.Orville.Oracle.Internal.FromClause
import Database.Orville.Oracle.Internal.FromSql
import Database.Orville.Oracle.Internal.MigrateConstraint
import Database.Orville.Oracle.Internal.MigrateIndex
import Database.Orville.Oracle.Internal.MigrateTable
import Database.Orville.Oracle.Internal.MigrationError
import Database.Orville.Oracle.Internal.MigrationPlan
import Database.Orville.Oracle.Internal.Monad
import Database.Orville.Oracle.Internal.SchemaState
import Database.Orville.Oracle.Internal.Select
import Database.Orville.Oracle.Internal.Types
import Database.Orville.Oracle.Raw
import Database.Orville.Oracle.Select

orvilleLockScope :: Int32
orvilleLockScope = 17772

migrationLockId :: Int32
migrationLockId = 7995632

tryLockExpr :: SelectExpr
tryLockExpr =
  rawSqlExpr $
  "pg_try_advisory_xact_lock(" <> fromString (show orvilleLockScope) <> "," <>
  fromString (show migrationLockId) <>
  ") as result"

waitForLockExpr :: SelectExpr
waitForLockExpr =
  rawSqlExpr $
  "pg_advisory_xact_lock(" <> fromString (show orvilleLockScope) <> "," <>
  fromString (show migrationLockId) <>
  ") as result"

lockResult :: FromSql Bool
lockResult = col ("result" :: String)

withLockedTransaction :: (MonadOrville conn m, MonadThrow m) => m a -> m a
withLockedTransaction action = do
  go (0 :: Int)
  where
    go attempts = do
      result <- runWithTransaction
      case result of
        Just a -> pure a
        Nothing -> do
          when (attempts >= 25) $ do
            throwM $
              MigrationLockExcessiveRetryError
                "Giving up after 25 attempts to aquire the migration lock."
          liftIO $ threadDelay 10000
          go $ attempts + 1
    runWithTransaction =
      withTransaction $ do
        [locked] <-
          runSelect $
          selectQueryColumns [tryLockExpr] lockResult (fromClauseRaw "") mempty
        if locked
          then Just <$> action
          else do
            void $
              runSelect $
              selectQueryColumns
                [waitForLockExpr]
                (pure ())
                (fromClauseRaw "")
                mempty
            pure Nothing

{-|
   migrateSchema will attempt to make changes to the actual database schema
   that it it matches the provided SchemaDefinition. Unsafe migrations such as
   dropping tables or columns are never attempted unless the SchemaDefinition
   explicitly states that the items are safe to drop. Column types may be changed,
   but will fail if the database cannot successfully make the request type change.
  -}
migrateSchema :: MonadOrville conn m => SchemaDefinition -> m ()
migrateSchema schemaDef =
  withConnection $ \conn -> do
    withLockedTransaction $ do
      plan <- nonTransactionallyGenerateMigrationPlan conn schemaDef
      case plan of
        Nothing -> pure ()
        Just somethingToDo ->
          nonTransactionallyExecuteMigrationPlan conn somethingToDo

{-|
   generateMigrationPlan inspects the state of the actual database schema and
   constructs a plan describing what changes would be made to make it match the
   provided SchemaDefinition. If the actual schema already matches the
   definition, Nothing will be returned.
 -}
generateMigrationPlan ::
     MonadOrville conn m => SchemaDefinition -> m (Maybe MigrationPlan)
generateMigrationPlan schemaDef =
  withConnection $ \conn -> do
    withLockedTransaction $ do
      nonTransactionallyGenerateMigrationPlan conn schemaDef

nonTransactionallyGenerateMigrationPlan ::
     MonadOrville conn m => conn -> SchemaDefinition -> m (Maybe MigrationPlan)
nonTransactionallyGenerateMigrationPlan conn schemaDef =
  liftIO $ buildMigrationPlan schemaDef <$> loadSchemaState conn

nonTransactionallyExecuteMigrationPlan ::
     MonadOrville conn m => conn -> MigrationPlan -> m ()
nonTransactionallyExecuteMigrationPlan conn plan = do
  forM_ (migrationPlanItems plan) $ \(MigrationItem schemaItem ddl) ->
    executingSql DDLQuery ddl $ do
      stmt <- prepare conn ddl
      executeRaw stmt `Exc.catch`
        (Exc.throw . MigrationExecutionError schemaItem)

buildMigrationPlan :: SchemaDefinition -> SchemaState -> Maybe MigrationPlan
buildMigrationPlan schemaDef schemaState = foldMap mkPlan schemaDef
  where
    mkPlan element =
      case element of
        Table tableDef -> migrateTablePlan tableDef schemaState
        DropTable name -> dropTablePlan name schemaState
        Index indexDef -> createIndexPlan indexDef schemaState
        DropIndex name -> dropIndexPlan name schemaState
        Constraint constraintDef ->
          createConstraintPlan constraintDef schemaState
        DropConstraint tablName name ->
          dropConstraintPlan tablName name schemaState
