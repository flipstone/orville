{-|
Module    : Database.Orville.PostgreSQL.Internal.MigrationPlan
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP #-}

module Database.Orville.PostgreSQL.Internal.MigrationPlan
  ( MigrationPlan
  , MigrationItem(..)
  , DDL
  , migrationDDLForItem
  , migrationPlanItems
  ) where

import qualified Data.DList as DList

import Database.Orville.PostgreSQL.Internal.Types (SchemaItem)

type DDL = String

{- |
  Migration Guide: @MigrationItem@ has been renamed to @MigrationStep@, which
  is now a simple @RawSql@ wrapper. You can use @RawSql.toExampleBytes@ if you
  wish to render it to a bytestring for display purposes.
-}
data MigrationItem = MigrationItem
  { migrationItemSchemaItem :: SchemaItem
  , migrationItemDDL :: DDL
  }

{- |
  Migration Guide: @MigrationPlan@ retains the same name.
-}
data MigrationPlan =
  MigrationPlan MigrationItem
                (DList.DList MigrationItem)

migrationDDLForItem :: SchemaItem -> DDL -> MigrationPlan
migrationDDLForItem schemaItem ddl =
  MigrationPlan (MigrationItem schemaItem ddl) DList.empty

append :: MigrationPlan -> MigrationPlan -> MigrationPlan
append (MigrationPlan itemA restA) (MigrationPlan itemB restB) =
  MigrationPlan itemA $ DList.append restA $ DList.cons itemB restB

{- |
  Migration Guide: @migrationPlanItems@ has been renamed to
  @migrationPlanSteps@
-}
migrationPlanItems :: MigrationPlan -> [MigrationItem]
migrationPlanItems (MigrationPlan item rest) =
  DList.toList $ DList.cons item rest

#if MIN_VERSION_base(4,11,0)
instance Semigroup MigrationPlan where
  (<>) = append
#else
instance Monoid MigrationPlan
  -- MigrationPlan doesn't support mempty, so don't provide a Monoid instance for
  -- base versions that have migrated to Semigroup.
  where
    mempty =
      error
        "mempty for MigrationPlan used, but MigrationPlan cannot be empty! MigrationPlan only support Monoid prior to base 4.11.0"
    mappend = append
#endif
