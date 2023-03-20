module Orville.PostgreSQL.PgCatalog.OidField
  ( oidField,
    oidTypeField,
  )
where

import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType

{- |
  The @oid@ field found on many (but not all!) @pg_catalog@ tables
-}
oidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
oidField =
  oidTypeField "oid"

{- |
  Builds a 'Orville.FieldDefinition' with the given column name that stores
  an @oid@ value.
-}
oidTypeField :: String -> Orville.FieldDefinition Orville.NotNull LibPQ.Oid
oidTypeField =
  Orville.fieldOfType SqlType.oid
