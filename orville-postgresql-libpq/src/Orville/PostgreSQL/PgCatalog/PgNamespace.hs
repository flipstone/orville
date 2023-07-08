{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.PgCatalog.PgNamespace
  ( PgNamespace (..)
  , NamespaceName
  , namespaceNameToString
  , pgNamespaceTable
  , namespaceNameField
  )
where

import qualified Data.String as String
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_namespace@
  table. Namespaces in @pg_catalog@ correspond to "schema" concept in database
  organization.
-}
data PgNamespace = PgNamespace
  { pgNamespaceOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the namespace. This is referenced from
  -- other tables, such as @pg_class@.
  , pgNamespaceName :: NamespaceName
  -- ^ The name of the namespace.
  }

{- |
  A Haskell type for the name of a namespace
-}
newtype NamespaceName
  = NamespaceName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Convert a 'NamespaceName to a plain 'String'
-}
namespaceNameToString :: NamespaceName -> String
namespaceNameToString (NamespaceName text) =
  T.unpack text

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_namespace@ table
-}
pgNamespaceTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgNamespace PgNamespace
pgNamespaceTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinition
      "pg_namespace"
      (Orville.primaryKey oidField)
      pgNamespaceMarshaller

pgNamespaceMarshaller :: Orville.SqlMarshaller PgNamespace PgNamespace
pgNamespaceMarshaller =
  PgNamespace
    <$> Orville.marshallField pgNamespaceOid oidField
    <*> Orville.marshallField pgNamespaceName namespaceNameField

{- |
  The @nspname@ column of the @pg_catalog.pg_namespace@ table
-}
namespaceNameField :: Orville.FieldDefinition Orville.NotNull NamespaceName
namespaceNameField =
  Orville.coerceField $
    Orville.unboundedTextField "nspname"
