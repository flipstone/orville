module Orville.PostgreSQL.Internal.IndexDefinition
  ( IndexDefinition,
    uniqueIndex,
    nonUniqueIndex,
    mkIndexDefinition,
    Expr.IndexUniqueness (UniqueIndex, NonUniqueIndex),
    IndexMigrationKey (IndexMigrationKey, indexKeyUniqueness, indexKeyColumns),
    indexMigrationKey,
    indexCreateExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDefinition

{- |
  Defines an index that can be added to a 'Orville.PostgreSQL.TableDefinition'.
  Use one of the constructor functions below (such as 'uniqueIndex') to
  construct the index definition you wish to have and then use
  'Orville.PostgreSQL.addTableIndexes'. to add them to your table definition.
  Orville will then add the index next time you run auto-migrations.
-}
data IndexDefinition = IndexDefinition
  { _indexCreateExpr :: Expr.QualifiedTableName -> Expr.CreateIndexExpr
  , _indexMigrationKey :: IndexMigrationKey
  }

{- |
  The key used by Orville to determine whether an index should be added to
  a table when performing auto migrations. For most use cases the constructor
  functions that build an 'IndexDefinition' will create this automatically
  for you.
-}
data IndexMigrationKey = IndexMigrationKey
  { indexKeyUniqueness :: Expr.IndexUniqueness
  , indexKeyColumns :: [FieldDefinition.FieldName]
  }
  deriving (Eq, Ord, Show)

{- |
  Gets the 'IndexMigrationKey' for the 'IndexDefinition'
-}
indexMigrationKey :: IndexDefinition -> IndexMigrationKey
indexMigrationKey = _indexMigrationKey

{- |
  Gets the SQL expression that will be used to add the index to the specified
  table.
-}
indexCreateExpr :: IndexDefinition -> Expr.QualifiedTableName -> Expr.CreateIndexExpr
indexCreateExpr = _indexCreateExpr

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.
-}
nonUniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
nonUniqueIndex =
  mkIndexDefinition Expr.NonUniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.
-}
uniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
uniqueIndex =
  mkIndexDefinition Expr.UniqueIndex

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
        IndexMigrationKey
          { indexKeyUniqueness = uniqueness
          , indexKeyColumns = NEL.toList fieldNames
          }
   in IndexDefinition
        { _indexCreateExpr = expr
        , _indexMigrationKey = migrationKey
        }
