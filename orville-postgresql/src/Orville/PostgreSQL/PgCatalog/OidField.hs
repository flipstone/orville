{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.PgCatalog.OidField
  ( oidField
  , oidTypeField
  )
where

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Marshall.SqlType as SqlType

{- | The @oid@ field found on many (but not all!) @pg_catalog@ tables.

@since 1.0.0.0
-}
oidField :: Orville.FieldDefinition Orville.NotNull Orville.Oid
oidField =
  oidTypeField "oid"

{- | Builds a 'Orville.FieldDefinition' with the given column name that stores
  an @oid@ value.

@since 1.0.0.0
-}
oidTypeField :: String -> Orville.FieldDefinition Orville.NotNull Orville.Oid
oidTypeField =
  Orville.fieldOfType SqlType.oid
