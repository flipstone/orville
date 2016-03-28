module Database.Orville.Internal.ConstraintDefinition
  ( uniqueConstraint
  , dropConstraint
  ) where

import            Data.List (intercalate)

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types

uniqueConstraint :: String -> TableDefinition entity -> [FieldDefinition] -> ConstraintDefinition
uniqueConstraint name tableDef fields =
  ConstraintDefinition {
    constraintName = name
  , constraintTable = tableName tableDef
  , constraintBody = "UNIQUE (" ++ intercalate "," (map escapedFieldName fields) ++ ")"
  }

dropConstraint :: TableDefinition entity -> String -> SchemaItem
dropConstraint tableDef = DropConstraint (tableName tableDef)

