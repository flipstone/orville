{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.PgCatalog.PgClass
  ( PgClass (..),
    RelationName,
    RelationKind (..),
    pgClassTable,
    relationNameField,
    namespaceOidField,
    relationKindField,
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
-}
data PgClass = PgClass
  { -- | The PostgreSQL @oid@ for the relation
    pgClassOid :: LibPQ.Oid
  , -- | The PostgreSQL @oid@ of the namespace that the relation belongs to.
    -- References @pg_namespace.oid@.
    pgClassNamespaceOid :: LibPQ.Oid
  , -- | The name of relation
    pgClassRelationName :: RelationName
  , -- | The kind of relation (table, view, etc)
    pgClassRelationKind :: RelationKind
  }

{- |
  A Haskell type for the name of the relation represented by a 'PgClass'
-}
newtype RelationName
  = RelationName T.Text
  deriving (Show, Eq, Ord, String.IsString)

{- |
  The kind of relation represented by a 'PgClass', as described at
  https://www.postgresql.org/docs/13/catalog-pg-class.html.
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
-}
namespaceOidField :: Orville.FieldDefinition Orville.NotNull LibPQ.Oid
namespaceOidField =
  oidTypeField "relnamespace"

{- |
  The @relname@ column of the @pg_catalog.pg_class@ table
-}
relationNameField :: Orville.FieldDefinition Orville.NotNull RelationName
relationNameField =
  Orville.coerceField $
    Orville.unboundedTextField "relname"

{- |
  The @relkind@ column of the @pg_catalog.pg_class@ table
-}
relationKindField :: Orville.FieldDefinition Orville.NotNull RelationKind
relationKindField =
  Orville.convertField
    (Orville.maybeConvertSqlType relationKindToPgText pgTextToRelationKind)
    (Orville.unboundedTextField "relkind")

{- |
  Converts a 'RelationKind' to the corresponding single character text
  representation used by PostgreSQL.

  See also 'pgTextToRelationKind'
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
-}
pgTextToRelationKind :: T.Text -> Maybe RelationKind
pgTextToRelationKind text =
  case T.unpack text of
    "r" -> Just OrdinaryTable
    "i" -> Just Index
    "S" -> Just Sequence
    "t" -> Just ToastTable
    "v" -> Just View
    "m" -> Just MaterializedView
    "c" -> Just CompositeType
    "f" -> Just ForeignTable
    "p" -> Just PartitionedTable
    "I" -> Just PartitionedIndex
    _ -> Nothing
