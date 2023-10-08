{-# LANGUAGE RankNTypes #-}
module Database.Orville.PostgreSQL.Internal.PrimaryKey
  ( primaryKeyIn
  , primaryKeyEquals
  , primaryKeyDescription
  , primaryKeyToSql
  , primaryKey
  , compositePrimaryKey
  , primaryKeyPart
  , mapPrimaryKeyParts
  ) where

import qualified Data.List as List
import qualified Database.HDBC as HDBC

import Database.Orville.PostgreSQL.Internal.FieldDefinition (fieldToSqlValue)
import Database.Orville.PostgreSQL.Internal.Types (PrimaryKey(..), PrimaryKeyPart(..), FieldDefinition(fieldName), NotNull)
import Database.Orville.PostgreSQL.Internal.Where (WhereCondition, whereIn, (.==), whereAnd, whereOr)

{-|
  'primaryKeyIn' builds a 'WhereCondition' that will match all rows where the
  primary key is equal to one of the given values. For single-field primary
  keys this is equivalent to 'whereIn', but 'primaryKeyIn' also handles
  composite primary keys.
-}
primaryKeyIn :: PrimaryKey key -> [key] -> WhereCondition
primaryKeyIn keyDef@(PrimaryKey first rest) keys =
  case rest of
    [] ->
      -- Special case the single field case to an in clause rather
      -- than a large OR
      case first of
        PrimaryKeyPart getPart field ->
          whereIn field (map getPart keys)
    _ ->
      whereOr (map (primaryKeyEquals keyDef) keys)

{-|
  'primaryKeyEquals' builds a 'WhereCondition' that will match the row where
  the primary key is equal to the given value. For single-field primary keys
  this is equivalent to '.==', but 'primaryKeyEquals also handles composite
  primary keys.
-}
primaryKeyEquals :: PrimaryKey key -> key -> WhereCondition
primaryKeyEquals keyDef key =
  let
    partEq getPart partField =
      partField .== getPart key
  in
    whereAnd (mapPrimaryKeyParts partEq keyDef)

{-|
  'primaryKeyDescription' builds a user-readable representation of the
  primary key for use in error messages and such. It is a comma-delimited
  list of the names of the fields that make up the primary key.
-}
primaryKeyDescription :: PrimaryKey key -> String
primaryKeyDescription keyDef =
  let
    partName _ field =
      fieldName field
  in
    List.intercalate ", " (mapPrimaryKeyParts partName keyDef)

{-|
  'primaryKeyToSql' converts a Haskell value for a primary key into the
  (possibly multiple) sql values that represent the primary key in the
  database.
-}
primaryKeyToSql :: PrimaryKey key -> key -> [HDBC.SqlValue]
primaryKeyToSql keyDef key =
  let
    partSqlValue getPart partField =
      fieldToSqlValue partField (getPart key)
  in
    mapPrimaryKeyParts partSqlValue keyDef

{-|
  'primaryKey' constructs a single-field primary key from the 'FieldDefinition'
  that corresponds to the primary key's column. This is generally used while
  building a 'TableDefinition'.
-}
primaryKey :: FieldDefinition NotNull key -> PrimaryKey key
primaryKey fieldDef =
  PrimaryKey (PrimaryKeyPart id fieldDef) []

{-|
  'compositePrimaryKey' constructs a multi-field primary key from the given
  parts, each of which corresponds to one field in the primary key.  You should
  use this while building a 'TableDefinition' for a table that you want to have
  a multi-column primary key. See 'primaryKeyPart' for how to build the parts
  to be passed as parameters. Note: there is no special significance to the
  first argument other than requiring that there is at least one field in the
  primary key.
-}
compositePrimaryKey :: PrimaryKeyPart key
                    -> [PrimaryKeyPart key]
                    -> PrimaryKey key
compositePrimaryKey =
  PrimaryKey

{-|
  'primaryKeyPart' builds on section of a composite primary key based on the
  field definition that corresponds to that column of the primary key. The
  function given is used to decompose the Haskell value for the composite key
  into the individual parts so they can be converted to sql for things like
  building 'WhereCondition'
-}
primaryKeyPart :: (key -> part)
               -> FieldDefinition NotNull part
               -> PrimaryKeyPart key
primaryKeyPart =
  PrimaryKeyPart

{-|
  'mapPrimaryKeyParts' provides a way to access the innards of a 'PrimaryKey'
  definition to extract information. The given function will be called on
  each part of the primary key in order and the list of results is returned.
  Note that single-field and multi-field primary keys are treated the same by
  this function, with the single-field case simply behaving as composite key
  with just one part.
-}
mapPrimaryKeyParts :: (forall part.
                         (key -> part)
                        -> FieldDefinition NotNull part
                        -> a
                      )
                   -> PrimaryKey key
                   -> [a]
mapPrimaryKeyParts f (PrimaryKey first rest) =
  let
    doPart (PrimaryKeyPart getPart field) =
      f getPart field
  in
    map doPart (first:rest)
