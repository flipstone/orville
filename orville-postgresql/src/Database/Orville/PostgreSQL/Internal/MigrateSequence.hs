{-|
Module    : Database.Orville.PostgreSQL.Internal.MigrateSequence
Copyright : Flipstone Technology Partners 2016-2019
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.MigrateSequence
  ( createSequencePlan
  , dropSequencePlan
  ) where

import Control.Monad (guard)
import Data.List (intercalate)

import Database.Orville.PostgreSQL.Internal.MigrationPlan
import Database.Orville.PostgreSQL.Internal.SchemaState
import Database.Orville.PostgreSQL.Internal.Types

createSequencePlan :: SequenceDefinition -> SchemaState -> Maybe MigrationPlan
createSequencePlan seqDef schemaState = do
  guard (not $ schemaStateSequenceExists (sequenceName seqDef) schemaState)
  pure $
    migrationDDLForItem
      (Sequence seqDef)
      (intercalate
         " "
         [ "CREATE SEQUENCE"
         , sequenceName seqDef
         , maybe "" ("INCREMENT BY " ++) $ show <$> sequenceIncrement seqDef
         , maybe "" ("MINVALUE " ++) $ show <$> sequenceMinValue seqDef
         , maybe "" ("MAXVALUE " ++) $ show <$> sequenceMaxValue seqDef
         , maybe "" ("START WITH " ++) $ show <$> sequenceStart seqDef
         , maybe "" ("CACHE " ++) $ show <$> sequenceCache seqDef
         , cycleClause $ sequenceCycle seqDef
         ])
  where
    cycleClause True = "CYCLE"
    cycleClause _ = "NO CYCLE"

dropSequencePlan :: String -> SchemaState -> Maybe MigrationPlan
dropSequencePlan name schemaState = do
  guard (schemaStateSequenceExists name schemaState)
  pure $ migrationDDLForItem (DropSequence name) ("DROP SEQUENCE " ++ name)
