{-|
Module    : Database.Orville.PostgreSQL.Internal.MigrateSchema
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE DeriveDataTypeable #-}

module Database.Orville.PostgreSQL.Internal.MigrateSchema
  ( migrateSchema
  , generateMigrationPlan
  , createIndexesConcurrently
  , dropIndexesConcurrently
  ) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Exc
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Foldable
import Data.Int
import Data.List
import Data.String
import Database.HDBC hiding (withTransaction)

import Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))

import Database.Orville.PostgreSQL.Internal.Execute
import Database.Orville.PostgreSQL.Internal.Expr
import Database.Orville.PostgreSQL.Internal.FromClause
import Database.Orville.PostgreSQL.Internal.FromSql
import Database.Orville.PostgreSQL.Internal.MigrateConstraint
import Database.Orville.PostgreSQL.Internal.MigrateIndex
import Database.Orville.PostgreSQL.Internal.MigrateSequence
import Database.Orville.PostgreSQL.Internal.MigrateTable
import Database.Orville.PostgreSQL.Internal.MigrationError
import Database.Orville.PostgreSQL.Internal.MigrationPlan
import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.SchemaState
import Database.Orville.PostgreSQL.Internal.Select
import Database.Orville.PostgreSQL.Internal.Types
import Database.Orville.PostgreSQL.Raw
import Database.Orville.PostgreSQL.Select

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
   Migration Guide: @migrateSchema@ has been renamed to @autoMigrateSchema@

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
   Migration Guide: @generateMigrationPlan@ retains the same name. It has
   changed to always return a @MigrationPlan@. You can use check whether
   @migrationPlanSteps@ is as empty list if you wish to determine whether any
   migrations will be performed by the plan.


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
        Sequence seqDef ->
          createSequencePlan seqDef schemaState
        DropSequence name ->
          dropSequencePlan name schemaState

{-|
   Migration Plan: @createIndexesConcurrently@ has been removed. You should now
   use @setIndexCreationStrategy Asynchronous@ instead.

   createIndexesConcurrently will create the given indexes, if they do not exist using the
    PostgreSQL concurrently feature. However, this does *not* mean the the function happens
    concurrently. This will wait for PostgreSQL to return, but other operations to the table will be
    allowed during index creation.

   Note: PostgreSQL does not allow CREATE INDEX CONCURRENTLY to appear inside of a transaction. Use
   this function with care.
-}
createIndexesConcurrently :: MonadOrville conn m
                          => [IndexDefinition]
                          -> m ()
createIndexesConcurrently indexDefs =
  withConnection $ \conn -> do
    traverse_ (createIndexConcurrently conn) indexDefs

-- internal helper function that takes a connection and performs a single index creation.
createIndexConcurrently :: MonadOrville conn m
                        => conn
                        -> IndexDefinition
                        -> m ()
createIndexConcurrently conn indexDef =
  let ddl =
        (intercalate
          " "
          [ "CREATE"
          , if indexUnique indexDef
            then "UNIQUE"
            else ""
          , "INDEX"
          , "CONCURRENTLY"
          , "IF NOT EXISTS"
          , "\"" ++ indexName indexDef ++ "\""
          , "ON"
          , "\"" ++ indexTable indexDef ++ "\""
          , indexBody indexDef
          ])
  in
    executingSql DDLQuery ddl $ do
      stmt <- prepare conn ddl
      executeRaw stmt


{-|
   Migration Guide: @dropIndexesConcurrently@ has been removed.

   dropIndexesConcurrently will drop each of the given indexes with the CONCURRENTLY keyword,
   allowing for other table operations to continue while the index is dropped. However there are
   several caveats that come with this as noted at
   https://www.postgresql.org/docs/9.6/sql-dropindex.html . Much like 'createIndexesConcurrently'
   this cannot be used in a transaction. But further this cannot drop indexes that support UNIQUE or
   PRIMARY KEY constraints.

   Use this with care.
-}
dropIndexesConcurrently :: MonadOrville conn m
                        => [String]
                        -> m ()
dropIndexesConcurrently idxNames =
  withConnection $ \conn -> do
    traverse_ (dropIndexConcurrently conn) idxNames

dropIndexConcurrently :: MonadOrville conn m
                      => conn
                      -> String
                      -> m ()
dropIndexConcurrently conn idxName =
  let ddl =
        (intercalate
          " "
          [ "DROP"
          , "INDEX"
          , "CONCURRENTLY"
          , "IF EXISTS"
          , "\"" ++ idxName ++ "\""
          ])
  in
    executingSql DDLQuery ddl $ do
      stmt <- prepare conn ddl
      executeRaw stmt
