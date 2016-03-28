{-# LANGUAGE RankNTypes #-}
module Database.Orville.Internal.MigrateSchema
  ( migrateSchema
  ) where

import            Control.Monad
import            Control.Monad.IO.Class
import            Data.Convertible
import            Data.Int
import            Database.HDBC hiding (withTransaction)

import            Database.Orville.Internal.MigrateConstraint
import            Database.Orville.Internal.MigrateIndex
import            Database.Orville.Internal.MigrateTable
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.Types
import            Database.Orville.Raw

orvilleLockScope :: Int32
orvilleLockScope = 17772

migrationLockId :: Int32
migrationLockId = 7995632

migrateSchema :: SchemaDefinition -> Orville ()
migrateSchema schemaDef = withConnection $ \conn -> do
  withTransaction $ liftIO $ do
    void $ run conn
               "SELECT pg_advisory_xact_lock(?,?)"
               [convert orvilleLockScope, convert migrationLockId]

    tables <- getTables conn
    indexes <- getIndexes conn
    constraints <- getConstraints conn

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

      DropConstraint tableName name ->
        when (name `elem` constraints)
             (dropConstraint conn tableName name)

