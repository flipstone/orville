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
import Database.Orville.PostgreSQL.Internal.Where (WhereCondition, (.<-), (.==), whereAnd, whereOr)

primaryKeyIn :: PrimaryKey key -> [key] -> WhereCondition
primaryKeyIn keyDef@(PrimaryKey first rest) keys =
  case rest of
    [] ->
      -- Special case the single field case to an in clause rather
      -- than a large OR
      case first of
        PrimaryKeyPart getPart field ->
          field .<- map getPart keys
    _ ->
      whereOr (map (primaryKeyEquals keyDef) keys)

primaryKeyEquals :: PrimaryKey key -> key -> WhereCondition
primaryKeyEquals keyDef key =
  let
    partEq getPart partField =
      partField .== getPart key
  in
    whereAnd (mapPrimaryKeyParts partEq keyDef)

primaryKeyDescription :: PrimaryKey key -> String
primaryKeyDescription keyDef =
  let
    partName _ field =
      fieldName field
  in
    List.intercalate ", " (mapPrimaryKeyParts partName keyDef)

primaryKeyToSql :: PrimaryKey key -> key -> [HDBC.SqlValue]
primaryKeyToSql keyDef key =
  let
    partSqlValue getPart partField =
      fieldToSqlValue partField (getPart key)
  in
    mapPrimaryKeyParts partSqlValue keyDef

primaryKey :: FieldDefinition NotNull key -> PrimaryKey key
primaryKey fieldDef =
  PrimaryKey (PrimaryKeyPart id fieldDef) []

compositePrimaryKey :: PrimaryKeyPart key
                    -> [PrimaryKeyPart key]
                    -> PrimaryKey key
compositePrimaryKey =
  PrimaryKey

primaryKeyPart :: (key -> part)
               -> FieldDefinition NotNull part
               -> PrimaryKeyPart key
primaryKeyPart =
  PrimaryKeyPart

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
