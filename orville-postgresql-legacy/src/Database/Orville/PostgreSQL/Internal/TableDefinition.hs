{-|
Module    : Database.Orville.PostgreSQL.Internal.TableDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.TableDefinition where

import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.Types

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
