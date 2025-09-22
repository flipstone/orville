{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgProc
  ( PgProc (..)
  , ProcName
  , pgProcTable
  , procNameField
  , procNamespaceOidField
  ) where

import qualified Data.String as String
import qualified Data.Text as T

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)

{- | The Haskell representation of data read from the @pg_catalog.pg_proc@ table.
  Rows in this table reperesent functions, procedures, aggregate functions, and
  window functions (collectively also known as routines), such as those created
  via @CREATE FUNCTION@ or @CREATE PROCEDUCE.

@since 1.1.0.0
-}
data PgProc = PgProc
  { pgProcOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the relation.
  , pgProcNamespaceOid :: LibPQ.Oid
  {- ^ The PostgreSQL @oid@ of the namespace that the relation belongs to.
  References @pg_namespace.oid@.
  -}
  , pgProcName :: ProcName
  -- ^ The name of the proceduce or function.
  , pgProcSource :: T.Text
  {- ^ How the function is executed by the PostgreSQL function handler. This
  made the actual source code for interpreted languages, but could be almost
  anything depending on the language.
  -}
  }

{- | A Haskell type for the name of the trigger represented by a 'PgProc'.

@since 1.1.0.0
-}
newtype ProcName
  = ProcName T.Text
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

{- | An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_proc@ table.

@since 1.1.0.0
-}
pgProcTable :: Orville.TableDefinition Orville.NoKey PgProc PgProc
pgProcTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinitionWithoutKey
      "pg_proc"
      pgProcMarshaller

pgProcMarshaller :: Orville.SqlMarshaller PgProc PgProc
pgProcMarshaller =
  PgProc
    <$> Orville.marshallField pgProcOid oidField
    <*> Orville.marshallField pgProcNamespaceOid procNamespaceOidField
    <*> Orville.marshallField pgProcName procNameField
    <*> Orville.marshallField pgProcSource procSourceField

{- | The @pronamespace@ column of the @pg_catalog.pg_proc@ table.

@since 1.0.0.0
-}
procNamespaceOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
procNamespaceOidField =
  oidTypeField "pronamespace"

{- | The @proname@ column of the @pg_catalog.pg_proc@ table.

@since 1.1.0.0
-}
procNameField :: Orville.FieldDefinition Orville.NotNull ProcName
procNameField =
  Orville.coerceField $
    Orville.unboundedTextField "proname"

{- | The @prosrc@ column of the @pg_catalog.pg_proc@ table.

@since 1.1.0.0
-}
procSourceField :: Orville.FieldDefinition Orville.NotNull T.Text
procSourceField =
  Orville.unboundedTextField "prosrc"
