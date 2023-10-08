{-|
Module    : Database.Orville.PostgreSQL.Internal.MigrateIndex
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.PostgreSQL.Internal.MigrateIndex
  ( createIndexPlan
  , dropIndexPlan
  ) where

import Control.Monad
import Data.List

import Database.Orville.PostgreSQL.Internal.MigrationPlan
import Database.Orville.PostgreSQL.Internal.SchemaState
import Database.Orville.PostgreSQL.Internal.Types

createIndexPlan :: IndexDefinition -> SchemaState -> Maybe MigrationPlan
createIndexPlan indexDef schemaState = do
  guard (not $ schemaStateIndexExists (indexName indexDef) schemaState)
  pure $
    migrationDDLForItem
      (Index indexDef)
      (intercalate
         " "
         [ "CREATE"
         , if indexUnique indexDef
             then "UNIQUE"
             else ""
         , "INDEX"
         , "\"" ++ indexName indexDef ++ "\""
         , "ON"
         , "\"" ++ indexTable indexDef ++ "\""
         , indexBody indexDef
         ])

dropIndexPlan :: String -> SchemaState -> Maybe MigrationPlan
dropIndexPlan name schemaState = do
  guard (schemaStateIndexExists name schemaState)
  -- Here we drop the index only if it exists so that during the migration plan
  -- should something else prior cause the index to be dropped we will not cause an error.
  pure $ migrationDDLForItem (DropIndex name) ("DROP INDEX IF EXISTS " ++ "\"" ++ name ++ "\"")
