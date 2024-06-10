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

data PgDescription = PgDescription
  { pgDescriptionObjOid :: LibPQ.Oid
  , pgDescriptionClassOid :: LibPQ.Oid
  , pgDescriptionObjSubId :: Int32
  , pgDescriptionDescription :: T.Text
  }

pgDescriptionTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgDescription PgDescription
pgDescriptionTable =
  Orville.mkTableDefinition "pg_description" (Orville.primaryKey objOidField) pgDescriptionMarshaller

pgDescriptionMarshaller :: Orville.SqlMarshaller PgDescription PgDescription
pgDescriptionMarshaller =
  PgDescription
    <$> Orville.marshallField pgDescriptionObjOid objOidField
    <*> Orville.marshallField pgDescriptionClassOid classOidField
    <*> Orville.marshallField pgDescriptionObjSubId objSubIdField
    <*> Orville.marshallField pgDescriptionDescription descriptionField

objOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
objOidField = oidTypeField "objoid"

classOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
classOidField = oidTypeField "classoid"

objSubIdField :: Orville.FieldDefinition Orville.NotNull Int32
objSubIdField = Orville.integerField "objsubid"

descriptionField :: Orville.FieldDefinition Orville.NotNull T.Text
descriptionField = Orville.unboundedTextField "description"
