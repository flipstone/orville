{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgTrigger
  ( PgTrigger (..)
  , TriggerName
  , triggerNameToString
  , pgTriggerTable
  , triggerRelationOidField
  ) where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)

{- | The Haskell representation of data read from the @pg_catalog.pg_trigger@
  table. Rows in this table correspond to triggers created on tables and views.
@since 1.1.0.0
-}
data PgTrigger = PgTrigger
  { pgTriggerOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the relation.
  , pgTriggerRelationOid :: LibPQ.Oid
  {- ^ The PostgreSQL @oid@ of the relation that the trigger is onto.
  References @pg_class.oid@.
  -}
  , pgTriggerName :: TriggerName
  -- ^ The name of the trigger.
  , pgTriggerIsInternal :: Bool
  -- ^ The whether the trigger is internal accounting to PostgreSQL.
  }

{- | A Haskell type for the name of the trigger represented by a 'PgTrigger'.

@since 1.1.0.0
-}
newtype TriggerName
  = TriggerName T.Text
  deriving
    ( -- | @since 1.1.0.0
      Show
    , -- | @since 1.1.0.0
      Eq
    , -- | @since 1.1.0.0
      Ord
    , -- | @since 1.1.0.0
      String.IsString
    )

{- | Convert a 'TriggerName' to a plain 'String'.

@since 1.1.0.0
-}
triggerNameToString :: TriggerName -> String
triggerNameToString (TriggerName name) =
  T.unpack name

{- | An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_trigger@ table.

@since 1.1.0.0
-}
pgTriggerTable :: Orville.TableDefinition Orville.NoKey PgTrigger PgTrigger
pgTriggerTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_trigger"
      pgTriggerMarshaller

pgTriggerMarshaller :: Orville.SqlMarshaller PgTrigger PgTrigger
pgTriggerMarshaller =
  PgTrigger
    <$> Orville.marshallField pgTriggerOid oidField
    <*> Orville.marshallField pgTriggerRelationOid triggerRelationOidField
    <*> Orville.marshallField pgTriggerName triggerNameField
    <*> Orville.marshallField pgTriggerIsInternal triggerIsInternalField

{- | The @tgrelid@ column of the @pg_trigger@ table.

@since 1.1.0.0
-}
triggerRelationOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
triggerRelationOidField =
  oidTypeField "tgrelid"

{- | The @tgname@ column of the @pg_catalog.pg_trigger@ table.

@since 1.1.0.0
-}
triggerNameField :: Orville.FieldDefinition Orville.NotNull TriggerName
triggerNameField =
  Orville.coerceField $
    Orville.unboundedTextField "tgname"

{- | The @tgisinternal@ column of the @pg_catalog.pg_trigger@ table.

@since 1.1.0.0
-}
triggerIsInternalField :: Orville.FieldDefinition Orville.NotNull Bool
triggerIsInternalField =
  Orville.booleanField "tgisinternal"
