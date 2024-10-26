{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgDescription
  ( PgDescription (..)
  , ObjectSubId
  , objectSubIdFromAttributeNumber
  , objectSubIdZero
  , pgDescriptionTable
  , objOidField
  , objSubIdField
  , descriptionField
  ) where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidTypeField)
import Orville.PostgreSQL.PgCatalog.PgAttribute (AttributeNumber, attributeNumberToInt16)

{- | The Haskell representation of data read from the @pg_catalog.pg_description@
  table.

@since 1.1.0.0
-}
data PgDescription = PgDescription
  { pgDescriptionObjOid :: LibPQ.Oid
  -- ^ @since 1.1.0.0
  , pgDescriptionClassOid :: LibPQ.Oid
  -- ^ @since 1.1.0.0
  , pgDescriptionObjSubId :: ObjectSubId
  -- ^ @since 1.1.0.0
  , pgDescriptionDescription :: T.Text
  -- ^ @since 1.1.0.0
  }

{- | Represents the value in the @objsubid@ field. For tables, this corresponds to the
  'AttributeNumber' of the column and indicates that the @description@ field contains
  the comment for that column.

  Note that the @objsubid@ field is an @int4@ as opposed to an @int2@ like the @attnum@
  field in @pg_attribute@.

@since 1.1.0.0
-}
newtype ObjectSubId = ObjectSubId Int32
  deriving
    ( -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    )

{- | Convert an 'AttributeNumber' to an 'ObjectSubId'

@since 1.1.0.0
-}
objectSubIdFromAttributeNumber :: AttributeNumber -> ObjectSubId
objectSubIdFromAttributeNumber = ObjectSubId . fromIntegral . attributeNumberToInt16

{- | A 'ObjectSubId' of 0 in the @objsubid@ field for a table corresponds to the
  comment on the table.

@since 1.1.0.0
-}
objectSubIdZero :: ObjectSubId
objectSubIdZero = ObjectSubId 0

-- | @since 1.1.0.0
pgDescriptionTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgDescription PgDescription
pgDescriptionTable =
  Orville.mkTableDefinition "pg_description" (Orville.primaryKey objOidField) pgDescriptionMarshaller

-- | @since 1.1.0.0
pgDescriptionMarshaller :: Orville.SqlMarshaller PgDescription PgDescription
pgDescriptionMarshaller =
  PgDescription
    <$> Orville.marshallField pgDescriptionObjOid objOidField
    <*> Orville.marshallField pgDescriptionClassOid classOidField
    <*> Orville.marshallField pgDescriptionObjSubId objSubIdField
    <*> Orville.marshallField pgDescriptionDescription descriptionField

-- | @since 1.1.0.0
objOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
objOidField = oidTypeField "objoid"

-- | @since 1.1.0.0
classOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
classOidField = oidTypeField "classoid"

-- | @since 1.1.0.0
objSubIdField :: Orville.FieldDefinition Orville.NotNull ObjectSubId
objSubIdField = Orville.coerceField $ Orville.integerField "objsubid"

-- | @since 1.1.0.0
descriptionField :: Orville.FieldDefinition Orville.NotNull T.Text
descriptionField = Orville.unboundedTextField "description"
