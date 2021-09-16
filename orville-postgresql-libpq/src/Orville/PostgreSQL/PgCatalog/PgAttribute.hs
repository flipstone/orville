{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.PgCatalog.PgAttribute
  ( PgAttribute (..),
    AttributeName,
    isOrdinaryColumn,
    pgAttributeTable,
    relationOidField,
    attributeNameField,
    attributeTypeOidField,
    attributeLengthField,
  )
where

import Data.Int (Int16)
import qualified Data.String as String
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidTypeField)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_attribute@
  table. Rows in this table correspond to table columns, but also for attributes
  of other items from the @pg_class@ table.

  See also 'Orville.PostgreSQL.PgCatalog.PgClass'.
-}
data PgAttribute = PgAttribute
  { -- | The PostgreSQL @oid@ for the relation that this
    -- attribute belongs to. References @pg_class.oid@
    pgAttributeRelationOid :: LibPQ.Oid
  , -- | The name of attribute
    pgAttributeName :: AttributeName
  , -- | The PostgreSQL number of attribute
    pgAttributeNumber :: AttributeNumber
  , -- | The PostgreSQL @oid@ for the type of this attribute. References
    -- @pg_type.oid@
    pgAttributeTypeOid :: LibPQ.Oid
  , -- | The length of this attributes type (a copy of @pg_type.typlen@)
    pgAttributeLength :: Int16
  }

{- |
  Determines whether the attribute represents a system column by inspecting
  the attribute\'s 'AttributeNumber'. Ordinary columns have attribute numbers
  starting at 1.
-}
isOrdinaryColumn :: PgAttribute -> Bool
isOrdinaryColumn attr =
  pgAttributeNumber attr > AttributeNumber 0

{- |
  A Haskell type for the name of the attribute represented by a 'PgAttribute'
-}
newtype AttributeName
  = AttributeName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  A Haskell type for the number of the attribute represented by a 'PgAttribute'
-}
newtype AttributeNumber
  = AttributeNumber Int16
  deriving (Eq, Ord)

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_attribute@ table
-}
pgAttributeTable :: Orville.TableDefinition Orville.NoKey PgAttribute PgAttribute
pgAttributeTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_attribute"
      pgAttributeMarshaller

pgAttributeMarshaller :: Orville.SqlMarshaller PgAttribute PgAttribute
pgAttributeMarshaller =
  PgAttribute
    <$> Orville.marshallField pgAttributeRelationOid relationOidField
    <*> Orville.marshallField pgAttributeName attributeNameField
    <*> Orville.marshallField pgAttributeNumber attributeNumberField
    <*> Orville.marshallField pgAttributeTypeOid attributeTypeOidField
    <*> Orville.marshallField pgAttributeLength attributeLengthField

{- |
  The @attrelid@ column of the @pg_catalog.pg_attribute@ table
-}
relationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
relationOidField =
  oidTypeField "attrelid"

{- |
  The @attname@ column of the @pg_catalog.pg_attribute@ table
-}
attributeNameField :: Orville.FieldDefinition Orville.NotNull AttributeName
attributeNameField =
  Orville.coerceField $
    Orville.unboundedTextField "attname"

{- |
  The @attnum@ column of the @pg_catalog.pg_attribute@ table
-}
attributeNumberField :: Orville.FieldDefinition Orville.NotNull AttributeNumber
attributeNumberField =
  Orville.coerceField $
    Orville.smallIntegerField "attnum"

{- |
  The @atttypid@ column of the @pg_catalog.pg_attribute@ table
-}
attributeTypeOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
attributeTypeOidField =
  oidTypeField "atttypid"

{- |
  The @attlen@ column of the @pg_catalog.pg_attribute@ table
-}
attributeLengthField :: Orville.FieldDefinition Orville.NotNull Int16
attributeLengthField =
  Orville.smallIntegerField "attlen"
