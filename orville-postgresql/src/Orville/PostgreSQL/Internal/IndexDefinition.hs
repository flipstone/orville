{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Internal.IndexDefinition
  ( IndexDefinition
  , indexCreationStrategy
  , setIndexCreationStrategy
  , uniqueIndex
  , uniqueNamedIndex
  , nonUniqueIndex
  , nonUniqueNamedIndex
  , mkIndexDefinition
  , mkNamedIndexDefinition
  , IndexMigrationKey (AttributeBasedIndexKey, NamedIndexKey)
  , AttributeBasedIndexMigrationKey (AttributeBasedIndexMigrationKey, indexKeyUniqueness, indexKeyColumns)
  , NamedIndexMigrationKey
  , indexMigrationKey
  , indexCreateExpr
  , IndexCreationStrategy (Transactional, Asynchronous)
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NEL

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.FieldDefinition as FieldDefinition

{- |
  Defines an index that can be added to a 'Orville.PostgreSQL.TableDefinition'.
  Use one of the constructor functions below (such as 'uniqueIndex') to
  construct the index definition you wish to have and then use
  'Orville.PostgreSQL.addTableIndexes'. to add them to your table definition.
  Orville will then add the index next time you run auto-migrations.

@since 1.0.0.0
-}
data IndexDefinition = IndexDefinition
  { i_indexCreateExpr ::
      IndexCreationStrategy ->
      Expr.Qualified Expr.TableName ->
      Expr.CreateIndexExpr
  , i_indexMigrationKey :: IndexMigrationKey
  , i_indexCreationStrategy :: IndexCreationStrategy
  }

{- |
  Sets the 'IndexCreationStrategy' strategy to be used when creating the index
  described by the 'IndexDefinition'. By default all indexes are created using
  the 'Transactional' strategy, but some tables are too large for for this to
  be feasible. See the 'Asynchronous' creation strategy for how to work around
  this.

@since 1.0.0.0
-}
setIndexCreationStrategy ::
  IndexCreationStrategy ->
  IndexDefinition ->
  IndexDefinition
setIndexCreationStrategy strategy indexDef =
  indexDef
    { i_indexCreationStrategy = strategy
    }

{- |
  Gets the 'IndexCreationStrategy' strategy to be used when creating the index
  described by the 'IndexDefinition'. By default all indexes are created using
  the 'Transactional' strategy.

@since 1.0.0.0
-}
indexCreationStrategy ::
  IndexDefinition ->
  IndexCreationStrategy
indexCreationStrategy =
  i_indexCreationStrategy

{- |
  Defines how an 'IndexDefinition' will be execute to add an index to a table.
  By default all indexes a created via the 'Transactional'

@since 1.0.0.0
-}
data IndexCreationStrategy
  = -- |
    --       The default strategy. The index will be added as part of the a database
    --       transaction along with all the other DDL being executed to migrate the
    --       database schema. If any migration should fail the index creation will be
    --       rolled back as part of the transaction. This is how schema migrations
    --       work in general in Orville.
    Transactional
  | -- |
    --       Creates the index asynchronously using the @CONCURRENTLY@ keyword in
    --       PostgreSQL. Index creation will return immediately and the index will be
    --       created in the background by PostgreSQL. Index creation may fail when
    --       using the 'Asynchronous' strategy. Orville has no special provision to
    --       detect or recover from this failure currently. You should manually check
    --       that index creation has succeeded. If necessary, you can manually drop
    --       the index to cause Orville to recreate it the next time migrations are
    --       run. This is useful when you need to add an index to a very large table
    --       because this can lock the table for a long time and cause an application
    --       to fail health checks at startup if migrations do not finish quickly enough.
    --       You should familiarize youself with how concurrent index creation works
    --       in PostgreSQL before using this. See
    --       https://www.postgresql.org/docs/current/sql-createindex.html#SQL-CREATEINDEX-CONCURRENTLY
    Asynchronous
  deriving (Eq, Show)

{- |
  Orville uses 'IndexMigrationKey' values while performing auto migrations to
  determine whether an index needs to be added or dropped. For most use cases
  the constructor functions that build an 'IndexDefinition' will create this
  automatically for you.

@since 1.0.0.0
-}
data IndexMigrationKey
  = AttributeBasedIndexKey AttributeBasedIndexMigrationKey
  | NamedIndexKey NamedIndexMigrationKey
  deriving (Eq, Ord)

{- |
  An 'IndexMigrationKey' using 'AttributeBasedIndexMigrationKey' will cause
  Orville to compare the structure of the indexes found in the database to the
  index structure it wants to create. If no matching index is found it will
  create a new index.

@since 1.0.0.0
-}
data AttributeBasedIndexMigrationKey = AttributeBasedIndexMigrationKey
  { indexKeyUniqueness :: Expr.IndexUniqueness
  , indexKeyColumns :: [FieldDefinition.FieldName]
  }
  deriving (Eq, Ord, Show)

{- |
  An 'IndexMigrationKey' using 'NamedIndexMigrationKey' will cause Orville to
  compare the only the names of indexes found in the database when determine
  whether to create the index. If an index with a matching name is found no
  index will be created. If no matching index name is found a new index will be
  created. This is often required when you create indexes using custom SQL
  where Orville is not able to do an accurate structural comparison of the
  desired index structure against the existing indexes.

@since 1.0.0.0
-}
type NamedIndexMigrationKey = String

{- |
  Gets the 'IndexMigrationKey' for the 'IndexDefinition'

@since 1.0.0.0
-}
indexMigrationKey :: IndexDefinition -> IndexMigrationKey
indexMigrationKey = i_indexMigrationKey

{- |
  Gets the SQL expression that will be used to add the index to the specified
  table.

@since 1.0.0.0
-}
indexCreateExpr :: IndexDefinition -> Expr.Qualified Expr.TableName -> Expr.CreateIndexExpr
indexCreateExpr indexDef =
  i_indexCreateExpr
    indexDef
    (i_indexCreationStrategy indexDef)

{- |
  Constructs an 'IndexDefinition' for a non-unique index on the given
  columns

@since 1.0.0.0
-}
nonUniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
nonUniqueIndex =
  mkIndexDefinition Expr.NonUniqueIndex

{- |
  Constructs an 'IndexDefinition' for a non-unique index with given SQL and
  index name

@since 1.0.0.0
-}
nonUniqueNamedIndex :: String -> Expr.IndexBodyExpr -> IndexDefinition
nonUniqueNamedIndex =
  mkNamedIndexDefinition Expr.NonUniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.

@since 1.0.0.0
-}
uniqueIndex :: NonEmpty FieldDefinition.FieldName -> IndexDefinition
uniqueIndex =
  mkIndexDefinition Expr.UniqueIndex

{- |
  Constructs an 'IndexDefinition' for a @UNIQUE@ index on the given
  columns.

@since 1.0.0.0
-}
uniqueNamedIndex :: String -> Expr.IndexBodyExpr -> IndexDefinition
uniqueNamedIndex =
  mkNamedIndexDefinition Expr.UniqueIndex

{- |
  Constructs an 'IndexDefinition' for an index on the given columns with the
  given uniquness.

@since 1.0.0.0
-}
mkIndexDefinition ::
  Expr.IndexUniqueness ->
  NonEmpty FieldDefinition.FieldName ->
  IndexDefinition
mkIndexDefinition uniqueness fieldNames =
  let
    expr strategy tableName =
      Expr.createIndexExpr
        uniqueness
        (mkMaybeConcurrently strategy)
        tableName
        (fmap FieldDefinition.fieldNameToColumnName fieldNames)

    migrationKey =
      AttributeBasedIndexMigrationKey
        { indexKeyUniqueness = uniqueness
        , indexKeyColumns = NEL.toList fieldNames
        }
  in
    IndexDefinition
      { i_indexCreateExpr = expr
      , i_indexMigrationKey = AttributeBasedIndexKey migrationKey
      , i_indexCreationStrategy = Transactional
      }

{- |
  Constructs an 'IndexDefinition' for an index with the given uniqueness, given
  name, and given SQL.

@since 1.0.0.0
-}
mkNamedIndexDefinition ::
  Expr.IndexUniqueness ->
  String ->
  Expr.IndexBodyExpr ->
  IndexDefinition
mkNamedIndexDefinition uniqueness indexName bodyExpr =
  let
    expr strategy tableName =
      Expr.createNamedIndexExpr
        uniqueness
        (mkMaybeConcurrently strategy)
        tableName
        (Expr.indexName indexName)
        bodyExpr
  in
    IndexDefinition
      { i_indexCreateExpr = expr
      , i_indexMigrationKey = NamedIndexKey indexName
      , i_indexCreationStrategy = Transactional
      }

{- |
  Internal helper to determine whether @CONCURRENTLY@ should be included in
  the SQL to create the index.

@since 1.0.0.0
-}
mkMaybeConcurrently :: IndexCreationStrategy -> Maybe Expr.ConcurrentlyExpr
mkMaybeConcurrently strategy =
  case strategy of
    Transactional -> Nothing
    Asynchronous -> Just Expr.concurrently
