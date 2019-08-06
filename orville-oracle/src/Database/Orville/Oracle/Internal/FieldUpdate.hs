{-|
Module    : Database.Orville.Oracle.Internal.FieldUpdate
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.Oracle.Internal.FieldUpdate where

import Database.Orville.Oracle.Internal.FieldDefinition
import Database.Orville.Oracle.Internal.Types

fieldUpdate :: FieldDefinition a -> a -> FieldUpdate
fieldUpdate fieldDef a =
  FieldUpdate (SomeField fieldDef) (fieldToSqlValue fieldDef a)

(.:=) :: FieldDefinition a -> a -> FieldUpdate
(.:=) = fieldUpdate

fieldUpdateName :: FieldUpdate -> String
fieldUpdateName = someFieldName . fieldUpdateField
  where
    someFieldName (SomeField f) = fieldName f
