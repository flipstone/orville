{-|
Module    : Database.Orville.Internal.FieldUpdate
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.Internal.FieldUpdate where

import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.Types

fieldUpdate :: FieldDefinition a -> a -> FieldUpdate
fieldUpdate fieldDef a =
  FieldUpdate (SomeField fieldDef) (fieldToSqlValue fieldDef a)

(.:=) :: FieldDefinition a -> a -> FieldUpdate
(.:=) = fieldUpdate

fieldUpdateName :: FieldUpdate -> String
fieldUpdateName = someFieldName . fieldUpdateField
  where
    someFieldName (SomeField f) = fieldName f
