{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Database.Orville.PostgreSQL.Internal.PrimaryKey
  ( PrimaryKey,
    primaryKeyDescription,
    primaryKeyToSql,
    primaryKey,
    compositePrimaryKey,
    primaryKeyPart,
    mapPrimaryKeyParts,
    mkPrimaryKeyExpr,
  )
where

import qualified Data.List as List

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, NotNull, fieldColumnName, fieldName, fieldNameToString, fieldValueToSqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A Haskell description of the 'FieldDefinition's that make up the primary
  key of a SQL table. This type supports composite primary keys as well
  as singular ones.
-}
data PrimaryKey key
  = PrimaryKey (PrimaryKeyPart key) [PrimaryKeyPart key]

{- |
  An internal helper type representing a single part of a composite primary
  key. This is not export in part to hide the use of GADTs, and also simply
  to avoid leaking implementation details.
-}
data PrimaryKeyPart key
  = forall part. PrimaryKeyPart (key -> part) (FieldDefinition NotNull part)

{- |
  'primaryKeyDescription' builds a user-readable representation of the
  primary key for use in error messages and such. It is a comma-delimited
  list of the names of the fields that make up the primary key.
-}
primaryKeyDescription :: PrimaryKey key -> String
primaryKeyDescription keyDef =
  let partName :: (part -> key) -> FieldDefinition NotNull a -> String
      partName _ field =
        fieldNameToString (fieldName field)
   in List.intercalate ", " (mapPrimaryKeyParts partName keyDef)

{- |
  'primaryKeyToSql' converts a Haskell value for a primary key into the
  (possibly multiple) sql values that represent the primary key in the
  database.
-}
primaryKeyToSql :: PrimaryKey key -> key -> [SqlValue.SqlValue]
primaryKeyToSql keyDef key =
  mapPrimaryKeyParts (partSqlValue key) keyDef

{- |
  'partSqlValue' is an internal helper function that builds the 'SqlValue'
  for one part of a (possible composite) primary key.
-}
partSqlValue :: key -> (key -> part) -> FieldDefinition NotNull part -> SqlValue.SqlValue
partSqlValue key getPart partField =
  fieldValueToSqlValue partField (getPart key)

{- |
  'primaryKey' constructs a single-field primary key from the 'FieldDefinition'
  that corresponds to the primary key's column. This is generally used while
  building a 'TableDefinition'.
-}
primaryKey :: FieldDefinition NotNull key -> PrimaryKey key
primaryKey fieldDef =
  PrimaryKey (PrimaryKeyPart id fieldDef) []

{- |
  'compositePrimaryKey' constructs a multi-field primary key from the given
  parts, each of which corresponds to one field in the primary key.  You should
  use this while building a 'TableDefinition' for a table that you want to have
  a multi-column primary key. See 'primaryKeyPart' for how to build the parts
  to be passed as parameters. Note: there is no special significance to the
  first argument other than requiring that there is at least one field in the
  primary key.
-}
compositePrimaryKey ::
  PrimaryKeyPart key ->
  [PrimaryKeyPart key] ->
  PrimaryKey key
compositePrimaryKey =
  PrimaryKey

{- |
  'primaryKeyPart' builds on section of a composite primary key based on the
  field definition that corresponds to that column of the primary key. The
  function given is used to decompose the Haskell value for the composite key
  into the individual parts so they can be converted to sql for things like
  building 'WhereCondition'
-}
primaryKeyPart ::
  (key -> part) ->
  FieldDefinition NotNull part ->
  PrimaryKeyPart key
primaryKeyPart =
  PrimaryKeyPart

{- |
  'mapPrimaryKeyParts' provides a way to access the innards of a 'PrimaryKey'
  definition to extract information. The given function will be called on
  each part of the primary key in order and the list of results is returned.
  Note that single-field and multi-field primary keys are treated the same by
  this function, with the single-field case simply behaving as composite key
  with just one part.
-}
mapPrimaryKeyParts ::
  ( forall part.
    (key -> part) ->
    FieldDefinition NotNull part ->
    a
  ) ->
  PrimaryKey key ->
  [a]
mapPrimaryKeyParts f (PrimaryKey first rest) =
  let doPart (PrimaryKeyPart getPart field) =
        f getPart field
   in map doPart (first : rest)

mkPrimaryKeyExpr :: PrimaryKey key -> Expr.PrimaryKeyExpr
mkPrimaryKeyExpr keyDef =
  let names =
        mapPrimaryKeyParts (\_ field -> fieldColumnName field) keyDef
   in Expr.primaryKeyExpr names
