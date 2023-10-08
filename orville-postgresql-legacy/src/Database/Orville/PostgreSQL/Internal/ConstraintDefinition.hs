{-|
Module    : Database.Orville.PostgreSQL.Internal.ContraintDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.ConstraintDefinition
  ( uniqueConstraint
  , dropConstraint
  ) where

import Data.List (intercalate)

import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.Types

{- |
  Migration Guide: @uniqueConstraint@ no longer accepts a name parameter.
  Instead the constraint is migrated automatically based on the structure of
  existing constraints found in the database. It also no longer accepts a
  @TableDefinition@. Instead you should use @addTableConstraints@ to add the
  @ConstraintDefinition@ to the table that you wish to apply the constraint to.
-}
uniqueConstraint ::
     String
  -> TableDefinition readEntity writeEntity key
  -> [SomeField]
  -> ConstraintDefinition
uniqueConstraint name tableDef fields =
  ConstraintDefinition
    { constraintName = name
    , constraintTable = tableName tableDef
    , constraintBody =
        "UNIQUE (" ++ intercalate "," (map someEscapedFieldName fields) ++ ")"
    }
  where
    someEscapedFieldName (SomeField f) = escapedFieldName f

{- |
  Migration Guide: @dropConstraint@ has been removed. Constraints are now
  dropped automatically during auto-migration when they are removed from the
  @TableDefinition@.
-}
dropConstraint ::
     TableDefinition readEntity writeEntity key -> String -> SchemaItem
dropConstraint tableDef = DropConstraint (tableName tableDef)
