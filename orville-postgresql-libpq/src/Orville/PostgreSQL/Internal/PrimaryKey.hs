{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Orville.PostgreSQL.Internal.PrimaryKey
  ( PrimaryKey,
    primaryKeyDescription,
    primaryKeyToSql,
    primaryKey,
    PrimaryKeyPart,
    compositePrimaryKey,
    primaryKeyPart,
    mapPrimaryKeyParts,
    mkPrimaryKeyExpr,
    primaryKeyEquals,
    primaryKeyEqualsExpr,
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), toList)

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, NotNull, fieldColumnName, fieldName, fieldNameToString, fieldValueToSqlValue)
import Orville.PostgreSQL.Internal.SelectOptions (WhereCondition, fieldEquals, whereAnd, whereConditionToBooleanExpr)
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

{- |
  A Haskell description of the 'FieldDefinition's that make up the primary
  key of a SQL table. This type supports composite primary keys as well
  as singular ones.
-}
data PrimaryKey key
  = PrimaryKey (PrimaryKeyPart key) [PrimaryKeyPart key]

{- |
  A 'PrimaryKeyPart' describes one field of a composite primary key. Values
  are built using 'primaryKeyPart' and then used with 'compositePrimaryKey'
  to build a 'PrimaryKey'
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
   in List.intercalate ", " (toList $ mapPrimaryKeyParts partName keyDef)

{- |
  'primaryKeyToSql' converts a Haskell value for a primary key into the
  (possibly multiple) sql values that represent the primary key in the
  database.
-}
primaryKeyToSql :: PrimaryKey key -> key -> NonEmpty SqlValue.SqlValue
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
  'primaryKeyPart' constructs a building block for a composite primary key
  based a 'FieldDefinition' and an accessor function to extract the value for
  that field from the Haskell 'key' type that represents the overall composite
  key.  'PrimaryKeyPart' values built using this function are usually then
  passed in a list to 'compositePrimaryKey' to build a 'PrimaryKey'.
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
  NonEmpty a
mapPrimaryKeyParts f (PrimaryKey first rest) =
  let doPart (PrimaryKeyPart getPart field) =
        f getPart field
   in fmap doPart (first :| rest)

{- |
  Builds a 'Expr.PrimaryKeyExpr' that is suitable to be used when creating
  a table to define the primary key on the table.
-}
mkPrimaryKeyExpr :: PrimaryKey key -> Expr.PrimaryKeyExpr
mkPrimaryKeyExpr keyDef =
  let names =
        mapPrimaryKeyParts (\_ field -> fieldColumnName field) keyDef
   in Expr.primaryKeyExpr names

{- |
  'primaryKeyEquals' builds a 'WhereCondition' that will match the row where
  the primary key is equal to the given value. For single-field primary keys
  this is equivalent to '.==', but 'primaryKeyEquals also handles composite
  primary keys.
-}
primaryKeyEquals :: PrimaryKey key -> key -> WhereCondition
primaryKeyEquals keyDef key =
  whereAnd (mapPrimaryKeyParts (partEquals key) keyDef)

{- |
  Like 'primaryKeyEquals', but returns the condition as a lower-level
  'Expr.BooleanExpr'
-}
primaryKeyEqualsExpr :: PrimaryKey key -> key -> Expr.BooleanExpr
primaryKeyEqualsExpr keyDef key =
  whereConditionToBooleanExpr (primaryKeyEquals keyDef key)

{- |
  INTERNAL: builds the where condition for a single part of the key
-}
partEquals :: key -> (key -> a) -> FieldDefinition nullability a -> WhereCondition
partEquals key getPart partField =
  fieldEquals partField (getPart key)
