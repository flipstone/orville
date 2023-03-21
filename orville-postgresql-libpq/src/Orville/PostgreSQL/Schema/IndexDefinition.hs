module Orville.PostgreSQL.Schema.IndexDefinition
  ( IndexDefinition,
    uniqueIndex,
    uniqueNamedIndex,
    nonUniqueIndex,
    nonUniqueNamedIndex,
    mkIndexDefinition,
    mkNamedIndexDefinition,
    Expr.IndexUniqueness (UniqueIndex, NonUniqueIndex),
    IndexMigrationKey (AttributeBasedIndexKey, NamedIndexKey),
    AttributeBasedIndexMigrationKey (AttributeBasedIndexMigrationKey, indexKeyUniqueness, indexKeyColumns),
    NamedIndexMigrationKey,
    indexMigrationKey,
    indexCreateExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDefinition

{- |
  Defines an index that can be added to a 'Orville.PostgreSQL.TableDefinition'.
  Use one of the constructor functions below (such as 'uniqueIndex') to
  construct the index definition you wish to have and then use
  'Orville.PostgreSQL.addTableIndexes'. to add them to your table definition.
  Orville will then add the index next time you run auto-migrations.
-}
data IndexDefinition = IndexDefinition
  { _indexCreateExpr :: Expr.Qualified Expr.TableName -> Expr.CreateIndexExpr
  , _indexMigrationKey :: IndexMigrationKey
  }

data IndexMigrationKey
  = AttributeBasedIndexKey AttributeBasedIndexMigrationKey
  | NamedIndexKey NamedIndexMigrationKey
  deriving (Eq, Ord)

{- |
  The key used by Orville to determine whether an index should be added to
  a table when performing auto migrations. For most use cases the constructor
  functions that build an 'IndexDefinition' will create this automatically
  for you.
-}
data AttributeBasedIndexMigrationKey = AttributeBasedIndexMigrationKey
  { indexKeyUniqueness :: Expr.IndexUniqueness
  , indexKeyColumns :: [FieldDefinition.FieldName]
  }
  deriving (Eq, Ord, Show)

type NamedIndexMigrationKey = String

{- |
  Gets the 'IndexMigrationKey' for the 'IndexDefinition'
-}
indexMigrationKey :: IndexDefinition -> IndexMigrationKey
indexMigrationKey = _indexMigrationKey

{- |
  Gets the SQL expression that will be used to add the index to the specified
  table.
-}
indexCreateExpr :: IndexDefinition -> Expr.Qualified Expr.TableName -> Expr.CreateIndexExpr
indexCreateExpr = _indexCreateExpr

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.
-}
nonUniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
nonUniqueIndex =
  mkIndexDefinition Expr.NonUniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index with given SQL and
  index name
-}
nonUniqueNamedIndex :: String -> RawSql.RawSql -> IndexDefinition
nonUniqueNamedIndex =
  mkNamedIndexDefinition Expr.NonUniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.
-}
uniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
uniqueIndex =
  mkIndexDefinition Expr.UniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.
-}
uniqueNamedIndex :: String -> RawSql.RawSql -> IndexDefinition
uniqueNamedIndex =
  mkNamedIndexDefinition Expr.UniqueIndex

{- |
  Constructs an 'IndexDefinition' for an index on the given columns with the
  given uniquness.
-}
mkIndexDefinition ::
  Expr.IndexUniqueness ->
  NonEmpty FieldDefinition.FieldName ->
  IndexDefinition
mkIndexDefinition uniqueness fieldNames =
  let expr tableName =
        Expr.createIndexExpr
          uniqueness
          tableName
          (fmap FieldDefinition.fieldNameToColumnName fieldNames)

      migrationKey =
        AttributeBasedIndexMigrationKey
          { indexKeyUniqueness = uniqueness
          , indexKeyColumns = NEL.toList fieldNames
          }
   in IndexDefinition
        { _indexCreateExpr = expr
        , _indexMigrationKey = AttributeBasedIndexKey migrationKey
        }

{- |
  Constructs an 'IndexDefinition' for an index with the given uniquness, given
  name, and given SQL.
-}
mkNamedIndexDefinition ::
  Expr.IndexUniqueness ->
  String ->
  RawSql.RawSql ->
  IndexDefinition
mkNamedIndexDefinition uniqueness indexName indexSql =
  let expr tableName =
        Expr.createNamedIndexExpr
          uniqueness
          tableName
          (Expr.indexName indexName)
          indexSql
   in IndexDefinition
        { _indexCreateExpr = expr
        , _indexMigrationKey = NamedIndexKey indexName
        }
