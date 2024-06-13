{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgDescription
  ( PgDescription (..)
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

{- |
  The Haskell representation of data read from the @pg_catalog.pg_description@
  table.

@since 1.1.0.0
-}
data PgDescription = PgDescription
  { pgDescriptionObjOid :: LibPQ.Oid
  -- ^ @since 1.1.0.0
  , pgDescriptionClassOid :: LibPQ.Oid
  -- ^ @since 1.1.0.0
  , pgDescriptionObjSubId :: Int32
  -- ^ @since 1.1.0.0
  , pgDescriptionDescription :: T.Text
  -- ^ @since 1.1.0.0
  }

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
objSubIdField :: Orville.FieldDefinition Orville.NotNull Int32
objSubIdField = Orville.integerField "objsubid"

-- | @since 1.1.0.0
descriptionField :: Orville.FieldDefinition Orville.NotNull T.Text
descriptionField = Orville.unboundedTextField "description"
