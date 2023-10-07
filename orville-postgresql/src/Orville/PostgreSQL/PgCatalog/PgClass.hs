{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.PgCatalog.PgClass
  ( PgClass (..)
  , RelationName
  , relationNameToString
  , RelationKind (..)
  , pgClassTable
  , relationNameField
  , namespaceOidField
  , relationKindField
  )
where

import qualified Data.String as String
import qualified Data.Text as T
import qualified Database.PostgreSQL.LibPQ as LibPQ

import qualified Orville.PostgreSQL as Orville
import Orville.PostgreSQL.PgCatalog.OidField (oidField, oidTypeField)

{- |
  The Haskell representation of data read from the @pg_catalog.pg_class@
  table. Rows in this table correspond to tables, indexes, sequences, views,
  materialized views, composite types and TOAST tables.

@since 1.0.0.0
-}
data PgClass = PgClass
  { pgClassOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ for the relation
  , pgClassNamespaceOid :: LibPQ.Oid
  -- ^ The PostgreSQL @oid@ of the namespace that the relation belongs to.
  -- References @pg_namespace.oid@.
  , pgClassRelationName :: RelationName
  -- ^ The name of relation
  , pgClassRelationKind :: RelationKind
  -- ^ The kind of relation (table, view, etc)
  }

{- |
  A Haskell type for the name of the relation represented by a 'PgClass'

@since 1.0.0.0
-}
newtype RelationName
  = RelationName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  Convert a 'RelationName' to a plain 'String'

@since 1.0.0.0
-}
relationNameToString :: RelationName -> String
relationNameToString (RelationName text) =
  T.unpack text

{- |
  The kind of relation represented by a 'PgClass', as described at
  https://www.postgresql.org/docs/13/catalog-pg-class.html.

@since 1.0.0.0
-}
data RelationKind
  = OrdinaryTable
  | Index
  | Sequence
  | ToastTable
  | View
  | MaterializedView
  | CompositeType
  | ForeignTable
  | PartitionedTable
  | PartitionedIndex
  deriving (Show, Eq)

{- |
  An Orville 'Orville.TableDefinition' for querying the
  @pg_catalog.pg_class@ table

@since 1.0.0.0
-}
pgClassTable :: Orville.TableDefinition (Orville.HasKey LibPQ.Oid) PgClass PgClass
pgClassTable =
  Orville.setTableSchema "pg_catalog" $
    Orville.mkTableDefinition
      "pg_class"
      (Orville.primaryKey oidField)
      pgClassMarshaller

pgClassMarshaller :: Orville.SqlMarshaller PgClass PgClass
pgClassMarshaller =
  PgClass
    <$> Orville.marshallField pgClassOid oidField
    <*> Orville.marshallField pgClassNamespaceOid namespaceOidField
    <*> Orville.marshallField pgClassRelationName relationNameField
    <*> Orville.marshallField pgClassRelationKind relationKindField

{- |
  The @relnamespace@ column of the @pg_catalog.pg_class@ table

@since 1.0.0.0
-}
namespaceOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
namespaceOidField =
  oidTypeField "relnamespace"

{- |
  The @relname@ column of the @pg_catalog.pg_class@ table

@since 1.0.0.0
-}
relationNameField :: Orville.FieldDefinition Orville.NotNull RelationName
relationNameField =
  Orville.coerceField $
    Orville.unboundedTextField "relname"

{- |
  The @relkind@ column of the @pg_catalog.pg_class@ table

@since 1.0.0.0
-}
relationKindField :: Orville.FieldDefinition Orville.NotNull RelationKind
relationKindField =
  Orville.convertField
    (Orville.tryConvertSqlType relationKindToPgText pgTextToRelationKind)
    (Orville.unboundedTextField "relkind")

{- |
  Converts a 'RelationKind' to the corresponding single character text
  representation used by PostgreSQL.

  See also 'pgTextToRelationKind'

@since 1.0.0.0
-}
relationKindToPgText :: RelationKind -> T.Text
relationKindToPgText kind =
  T.pack $
    case kind of
      OrdinaryTable -> "r"
      Index -> "i"
      Sequence -> "S"
      ToastTable -> "t"
      View -> "v"
      MaterializedView -> "m"
      CompositeType -> "c"
      ForeignTable -> "f"
      PartitionedTable -> "p"
      PartitionedIndex -> "I"

{- |
  Attempts to parse a PostgreSQL single character textual value as a
  'RelationKind'

  See also 'relationKindToPgText'

@since 1.0.0.0
-}
pgTextToRelationKind :: T.Text -> Either String RelationKind
pgTextToRelationKind text =
  case T.unpack text of
    "r" -> Right OrdinaryTable
    "i" -> Right Index
    "S" -> Right Sequence
    "t" -> Right ToastTable
    "v" -> Right View
    "m" -> Right MaterializedView
    "c" -> Right CompositeType
    "f" -> Right ForeignTable
    "p" -> Right PartitionedTable
    "I" -> Right PartitionedIndex
    kind -> Left ("Unrecognized PostgreSQL relation kind: " <> kind)
