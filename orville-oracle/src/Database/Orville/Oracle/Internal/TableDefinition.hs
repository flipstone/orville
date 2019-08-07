{-|
Module    : Database.Orville.Oracle.Internal.TableDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Internal.TableDefinition where

import Database.Orville.Oracle.Internal.FieldDefinition
import Database.Orville.Oracle.Internal.Types

tableColumnNames :: TableDefinition readEntity writeEntity key -> [String]
tableColumnNames = map someFieldName . tableFields
  where
    someFieldName (SomeField f) = fieldName f

tableAssignableFields ::
     TableDefinition readEntity writeEntity key -> [SomeField]
tableAssignableFields =
  filter (not . isSomeAssignedByDatabaseField) . tableFields
  where
    isSomeAssignedByDatabaseField (SomeField f) = isAssignedByDatabaseField f

tableAssignableColumnNames ::
     TableDefinition readEntity writeEntity key -> [String]
tableAssignableColumnNames = map someFieldName . tableAssignableFields
  where
    someFieldName (SomeField f) = fieldName f
