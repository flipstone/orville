{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Schema.PrimaryKey
  ( PrimaryKey
  , primaryKeyDescription
  , primaryKeyFieldNames
  , primaryKeyToSql
  , primaryKey
  , PrimaryKeyPart
  , compositePrimaryKey
  , primaryKeyPart
  , mapPrimaryKeyParts
  , mkPrimaryKeyExpr
  , primaryKeyEquals
  , primaryKeyIn
  )
where

import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)), toList)

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Extra.NonEmpty as ExtraNonEmpty
import Orville.PostgreSQL.Marshall.FieldDefinition (FieldDefinition, FieldName, NotNull, fieldEquals, fieldIn, fieldName,fieldNameToColumnName, fieldNameToString, fieldValueToSqlValue)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  A Haskell description of the 'FieldDefinition's that make up the primary
  key of a SQL table. This type supports composite primary keys as well
  as singular ones.

@since 1.0.0.0
-}
data PrimaryKey key
  = PrimaryKey (PrimaryKeyPart key) [PrimaryKeyPart key]

{- |
  A 'PrimaryKeyPart' describes one field of a composite primary key. Values
  are built using 'primaryKeyPart' and then used with 'compositePrimaryKey'
  to build a 'PrimaryKey'.

@since 1.0.0.0
-}
data PrimaryKeyPart key
  = forall part. PrimaryKeyPart (key -> part) (FieldDefinition NotNull part)

{- |
  'primaryKeyDescription' builds a user-readable representation of the
  primary key for use in error messages and such. It is a comma-delimited
  list of the names of the fields that make up the primary key.

@since 1.0.0.0
-}
primaryKeyDescription :: PrimaryKey key -> String
primaryKeyDescription =
  List.intercalate ", "
    . map fieldNameToString
    . toList
    . primaryKeyFieldNames

{- |
  Retrieves the names of the fields that are part of the primary key.

@since 1.0.0.0
-}
primaryKeyFieldNames :: PrimaryKey key -> NonEmpty FieldName
primaryKeyFieldNames =
  let
    partName :: (part -> key) -> FieldDefinition NotNull a -> FieldName
    partName _ field =
      fieldName field
  in
    mapPrimaryKeyParts partName

{- |
  'primaryKeyToSql' converts a Haskell value for a primary key into the
  (possibly multiple) SQL values that represent the primary key in the
  database.

@since 1.0.0.0
-}
primaryKeyToSql :: PrimaryKey key -> key -> NonEmpty SqlValue.SqlValue
primaryKeyToSql keyDef key =
  mapPrimaryKeyParts (partSqlValue key) keyDef

{- |
  'partSqlValue' is an internal helper function that builds the
  'SqlValue.SqlValue' for one part of a (possible composite) primary key.

@since 1.0.0.0
-}
partSqlValue :: key -> (key -> part) -> FieldDefinition NotNull part -> SqlValue.SqlValue
partSqlValue key getPart partField =
  fieldValueToSqlValue partField (getPart key)

{- |
  'primaryKey' constructs a single-field primary key from the 'FieldDefinition'
  that corresponds to the primary key's column. This is generally used while
  building a 'Orville.PostgreSQL.TableDefinition'.

@since 1.0.0.0
-}
primaryKey :: FieldDefinition NotNull key -> PrimaryKey key
primaryKey fieldDef =
  PrimaryKey (PrimaryKeyPart id fieldDef) []

{- |
  'compositePrimaryKey' constructs a multi-field primary key from the given
  parts, each of which corresponds to one field in the primary key. You should
  use this while building a 'Orville.PostgreSQL.TableDefinition' for a table
  that you want to have a multi-column primary key. See 'primaryKeyPart' for
  how to build the parts to be passed as parameters. Note: there is no special
  significance to the first argument other than requiring that there is at
  least one field in the primary key.

@since 1.0.0.0
-}
compositePrimaryKey ::
  PrimaryKeyPart key ->
  [PrimaryKeyPart key] ->
  PrimaryKey key
compositePrimaryKey =
  PrimaryKey

{- |
  'primaryKeyPart' constructs a building block for a composite primary key
  based on a 'FieldDefinition' and an accessor function to extract the value for
  that field from the Haskell @key@ type that represents the overall composite
  key. 'PrimaryKeyPart' values built using this function are usually then
  passed in a list to 'compositePrimaryKey' to build a 'PrimaryKey'.

@since 1.0.0.0
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
  this function, with the single-field case simply behaving as a composite key
  with just one part.

@since 1.0.0.0
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
  let
    doPart (PrimaryKeyPart getPart field) =
      f getPart field
  in
    fmap doPart (first :| rest)

{- |
  Builds a 'Expr.PrimaryKeyExpr' that is suitable to be used when creating
  a table to define the primary key on the table.

@since 1.0.0.0
-}
mkPrimaryKeyExpr :: PrimaryKey key -> Expr.PrimaryKeyExpr
mkPrimaryKeyExpr keyDef =
  let
    names =
      mapPrimaryKeyParts (\_ field -> fieldNameToColumnName $ fieldName field) keyDef
  in
    Expr.primaryKeyExpr names

{- |
  'primaryKeyEquals' builds a 'Expr.BooleanExpr' that will match the row where
  the primary key is equal to the given value. For single-field primary keys,
  this is equivalent to 'fieldEquals', but 'primaryKeyEquals' also handles
  composite primary keys.

@since 1.0.0.0
-}
primaryKeyEquals :: PrimaryKey key -> key -> Expr.BooleanExpr
primaryKeyEquals keyDef key =
  ExtraNonEmpty.foldl1'
    Expr.andExpr
    (mapPrimaryKeyParts (partEquals key) keyDef)

{- |
  'primaryKeyIn' builds a 'Expr.BooleanExpr' that will match rows where the
  primary key is contained in the given list. For single-field primary keys,
  this is equivalent to 'fieldIn', but 'primaryKeyIn' also handles composite
  primary keys.

@since 1.0.0.0
-}
primaryKeyIn :: PrimaryKey key -> NonEmpty key -> Expr.BooleanExpr
primaryKeyIn keyDef keys =
  case keyDef of
    PrimaryKey (PrimaryKeyPart getPart field) [] ->
      fieldIn field (fmap getPart keys)
    _ ->
      ExtraNonEmpty.foldl1'
        Expr.orExpr
        (fmap (primaryKeyEquals keyDef) keys)

{- |
  INTERNAL: builds the where condition for a single part of the key

@since 1.0.0.0
-}
partEquals :: key -> (key -> a) -> FieldDefinition nullability a -> Expr.BooleanExpr
partEquals key getPart partField =
  fieldEquals partField (getPart key)
