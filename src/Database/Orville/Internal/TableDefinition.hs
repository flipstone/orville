{-|
Module    : Database.Orville.Internal.TableDefinition
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Internal.TableDefinition where

import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.Types

tableColumnNames :: TableDefinition entity key -> [String]
tableColumnNames = map someFieldName . tableFields
  where
    someFieldName (SomeField f) = fieldName f

insertableColumnNames :: TableDefinition entity key -> [String]
insertableColumnNames =
  map someFieldName . filter (not . isSomeUninsertedField) . tableFields
  where
    isSomeUninsertedField (SomeField f) = isUninsertedField f
    someFieldName (SomeField f) = fieldName f
