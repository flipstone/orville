{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Orville.PostgreSQL.Schema.TableDefinition
  ( TableDefinition
  , HasKey
  , NoKey
  , mkTableDefinition
  , mkTableDefinitionWithoutKey
  , dropColumns
  , columnsToDrop
  , tableIdentifier
  , tableName
  , setTableSchema
  , tableConstraints
  , addTableConstraints
  , tableIndexes
  , addTableIndexes
  , tablePrimaryKey
  , tableMarshaller
  , mapTableMarshaller
  , mkInsertExpr
  , mkCreateTableExpr
  , mkTableColumnDefinitions
  , mkTablePrimaryKeyExpr
  , mkInsertColumnList
  , mkInsertSource
  , mkTableReturningClause
  )
where

import Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Orville.PostgreSQL.Execution.ReturningOption (ReturningOption (WithReturning, WithoutReturning))
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.FieldDefinition (fieldColumnDefinition, fieldColumnName, fieldValueToSqlValue)
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, MarshallerField (Natural, Synthetic), ReadOnlyColumnOption (ExcludeReadOnlyColumns, IncludeReadOnlyColumns), SqlMarshaller, annotateSqlMarshaller, annotateSqlMarshallerEmptyAnnotation, collectFromField, foldMarshallerFields, mapSqlMarshaller, marshallerDerivedColumns, unannotatedSqlMarshaller)
import Orville.PostgreSQL.Raw.SqlValue (SqlValue)
import Orville.PostgreSQL.Schema.ConstraintDefinition (ConstraintDefinition, ConstraintMigrationKey, constraintMigrationKey, constraintSqlExpr)
import Orville.PostgreSQL.Schema.IndexDefinition (IndexDefinition, IndexMigrationKey, indexMigrationKey)
import Orville.PostgreSQL.Schema.PrimaryKey (PrimaryKey, mkPrimaryKeyExpr, primaryKeyFieldNames)
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier, setTableIdSchema, tableIdQualifiedName, unqualifiedNameToTableId)

{- |
  Contains the definition of a SQL table for Orville to use for generating
  queries and marshalling Haskell values to and from the database.

  * 'key' is the Haskell type used to store values of the primary
    key for the table.

  * 'writeEntity' is the Haskell type for values that Orville will write
    to the database for you (i.e. both inserts and updates)

  * 'readEntity' is the Haskell type for values that Orville will decode
    from the result set when entities are queried from this table.
-}
data TableDefinition key writeEntity readEntity = TableDefinition
  { i_tableIdentifier :: TableIdentifier
  , i_tablePrimaryKey :: TablePrimaryKey key
  , i_tableMarshaller :: AnnotatedSqlMarshaller writeEntity readEntity
  , i_tableColumnsToDrop :: Set.Set String
  , i_tableConstraints :: Map.Map ConstraintMigrationKey ConstraintDefinition
  , i_tableIndexes :: Map.Map IndexMigrationKey IndexDefinition
  }

data HasKey key
data NoKey

data TablePrimaryKey tag where
  TableHasKey :: PrimaryKey key -> TablePrimaryKey (HasKey key)
  TableHasNoKey :: TablePrimaryKey NoKey

{- |
  Constructs a new 'TableDefinition' with the basic fields required for
  operation. For convenience, this function accepts a 'PrimaryKey' even though
  this is not required for all Orville operations to work. If you need to
  create a table without any primary key, see 'mkTableDefinitionWithoutKey'.
-}
mkTableDefinition ::
  -- | The name of the table
  String ->
  -- | Definition of the table's primary key
  PrimaryKey key ->
  -- | A 'SqlMarshaller' to marshall table entities to and from the database
  SqlMarshaller writeEntity readEntity ->
  TableDefinition (HasKey key) writeEntity readEntity
mkTableDefinition name primaryKey marshaller =
  TableDefinition
    { i_tableIdentifier = unqualifiedNameToTableId name
    , i_tablePrimaryKey = TableHasKey primaryKey
    , i_tableMarshaller = annotateSqlMarshaller (toList $ primaryKeyFieldNames primaryKey) marshaller
    , i_tableColumnsToDrop = Set.empty
    , i_tableConstraints = Map.empty
    , i_tableIndexes = Map.empty
    }

{- |
  Constructs a new 'TableDefinition' with the minimal fields required for
  operation. Note: tables created via this function will not have a primary
  key. Certain Orville functions required a primary key. Attempting to call
  functions requiring a primary key will fail to compile when using a table
  that has no key.
-}
mkTableDefinitionWithoutKey ::
  -- | The name of the table
  String ->
  -- | A 'SqlMarshaller' to marshall table entities to and from the database
  SqlMarshaller writeEntity readEntity ->
  TableDefinition NoKey writeEntity readEntity
mkTableDefinitionWithoutKey name marshaller =
  TableDefinition
    { i_tableIdentifier = unqualifiedNameToTableId name
    , i_tablePrimaryKey = TableHasNoKey
    , i_tableMarshaller = annotateSqlMarshallerEmptyAnnotation marshaller
    , i_tableColumnsToDrop = Set.empty
    , i_tableConstraints = Map.empty
    , i_tableIndexes = Map.empty
    }

{- |
  Annotates a 'TableDefinition' with a direction to drop columns if they are
  found in the database. Orville does not drop columns during auto migration
  unless they are explicitly requested to be dropped via 'dropColumns'.

  If you remove a reference to a column from the table's 'SqlMarshaller'
  without adding the column's name to 'dropColumns', Orville will operate as if
  the column does not exist without actually dropping the column. This is often
  useful if you're not sure you want to lose the data in the column, or if you
  have zero down-time deployments, which requires the column not be referenced
  by deployed code before it can be dropped.
-}
dropColumns ::
  -- | Columns that should be dropped from the table
  [String] ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
dropColumns columns tableDef =
  tableDef
    { i_tableColumnsToDrop = i_tableColumnsToDrop tableDef <> Set.fromList columns
    }

{- |
  Returns the set of columns that have be marked be dropped by 'dropColumns'
-}
columnsToDrop :: TableDefinition key writeEntity readEntity -> Set.Set String
columnsToDrop =
  i_tableColumnsToDrop

{- |
  Returns the table's 'TableIdentifier'
-}
tableIdentifier :: TableDefinition key writeEntity readEntity -> TableIdentifier
tableIdentifier =
  i_tableIdentifier

{- |
  Returns the table's name as an expression that can be used to build SQL
  statements. If the table has a schema name set, the name will be qualified
  with it.
-}
tableName :: TableDefinition key writeEntity readEntity -> Expr.Qualified Expr.TableName
tableName =
  tableIdQualifiedName . i_tableIdentifier

{- |
  Sets the table's schema to the name in the given string, which will be
  treated as a SQL identifier. If a table has a schema name set, it will be
  included as a qualified on the table name for all queries involving the
  table.
-}
setTableSchema ::
  String ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
setTableSchema schemaName tableDef =
  tableDef
    { i_tableIdentifier = setTableIdSchema schemaName (i_tableIdentifier tableDef)
    }

{- |
  Retrieves all the table constraints that have been added to the table via
  'addTableConstraints'.
-}
tableConstraints ::
  TableDefinition key writeEntity readEntity ->
  Map.Map ConstraintMigrationKey ConstraintDefinition
tableConstraints =
  i_tableConstraints

{- |
  Adds the given table constraints to the table definition.

  Note: If multiple constraints are added with the same
  'ConstraintMigrationKey', only the last one that is added will be part of the
  'TableDefinition'. Any previously added constraint with the same key is
  replaced by the new one.
-}
addTableConstraints ::
  [ConstraintDefinition] ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
addTableConstraints constraintDefs tableDef =
  let
    addConstraint constraint constraintMap =
      Map.insert (constraintMigrationKey constraint) constraint constraintMap
  in
    tableDef
      { i_tableConstraints = foldr addConstraint (i_tableConstraints tableDef) constraintDefs
      }

{- |
  Retrieves all the table indexes that have been added to the table via
  'addTableIndexes'.
-}
tableIndexes ::
  TableDefinition key writeEntity readEntity ->
  Map.Map IndexMigrationKey IndexDefinition
tableIndexes =
  i_tableIndexes

{- |
  Adds the given table indexes to the table definition.

  Note: If multiple indexes are added with the same 'IndexMigrationKey', only
  the last one that is added will be part of the 'TableDefinition'. Any
  previously added index with the same key is replaced by the new one.
-}
addTableIndexes ::
  [IndexDefinition] ->
  TableDefinition key writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
addTableIndexes indexDefs tableDef =
  let
    addIndex index indexMap =
      Map.insert (indexMigrationKey index) index indexMap
  in
    tableDef
      { i_tableIndexes = foldr addIndex (i_tableIndexes tableDef) indexDefs
      }

{- |
  Returns the primary key for the table, as defined at construction via 'mkTableDefinition'.
-}
tablePrimaryKey :: TableDefinition (HasKey key) writeEntity readEntity -> PrimaryKey key
tablePrimaryKey def =
  case i_tablePrimaryKey def of
    TableHasKey primaryKey -> primaryKey

{- |
  Returns the marshaller for the table, as defined at construction via 'mkTableDefinition'.
-}
tableMarshaller :: TableDefinition key writeEntity readEntity -> AnnotatedSqlMarshaller writeEntity readEntity
tableMarshaller = i_tableMarshaller

{- |
  Applies the provided function to the underlying 'SqlMarshaller' of the 'TableDefinition'
-}
mapTableMarshaller ::
  (SqlMarshaller readEntityA writeEntityA -> SqlMarshaller readEntityB writeEntityB) ->
  TableDefinition key readEntityA writeEntityA ->
  TableDefinition key readEntityB writeEntityB
mapTableMarshaller f tableDef =
  tableDef {i_tableMarshaller = mapSqlMarshaller f $ i_tableMarshaller tableDef}

{- |
  Builds a 'Expr.CreateTableExpr' that will create a SQL table matching the
  given 'TableDefinition' when it is executed.
-}
mkCreateTableExpr ::
  TableDefinition key writeEntity readEntity ->
  Expr.CreateTableExpr
mkCreateTableExpr tableDef =
  Expr.createTableExpr
    (tableName tableDef)
    (mkTableColumnDefinitions tableDef)
    (mkTablePrimaryKeyExpr tableDef)
    (map constraintSqlExpr . Map.elems . i_tableConstraints $ tableDef)

{- |
  Builds the 'Expr.ColumnDefinitions' for all the fields described by the
  table definition's 'SqlMarshaller'.
-}
mkTableColumnDefinitions ::
  TableDefinition key writeEntity readEntity ->
  [Expr.ColumnDefinition]
mkTableColumnDefinitions tableDef =
  foldMarshallerFields
    (unannotatedSqlMarshaller $ tableMarshaller tableDef)
    []
    (collectFromField IncludeReadOnlyColumns fieldColumnDefinition)

{- |
  Builds the 'Expr.PrimaryKeyExpr' for this table, or none of this table has no
  primary key.
-}
mkTablePrimaryKeyExpr ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.PrimaryKeyExpr
mkTablePrimaryKeyExpr tableDef =
  case i_tablePrimaryKey tableDef of
    TableHasKey primaryKey ->
      Just $ mkPrimaryKeyExpr primaryKey
    TableHasNoKey ->
      Nothing

{- |
  When 'WithReturning' is given, builds a 'Expr.ReturningExpr' that will
  return all the columns in the given table definition.
-}
mkTableReturningClause ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntty ->
  Maybe Expr.ReturningExpr
mkTableReturningClause returningOption tableDef =
  case returningOption of
    WithoutReturning ->
      Nothing
    WithReturning ->
      Just
        . Expr.returningExpr
        . Expr.selectDerivedColumns
        . marshallerDerivedColumns
        . unannotatedSqlMarshaller
        . tableMarshaller
        $ tableDef

{- |
  Builds an 'Expr.InsertExpr' that will insert the given entities into the SQL
  table when it is executed. A @RETURNING@ clause with either be included to
  return the insert rows or not, depending on the 'ReturnOption' given.
-}
mkInsertExpr ::
  ReturningOption returningClause ->
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Expr.InsertExpr
mkInsertExpr returningOption tableDef entities =
  let
    marshaller =
      unannotatedSqlMarshaller $ tableMarshaller tableDef

    insertColumnList =
      mkInsertColumnList marshaller

    insertSource =
      mkInsertSource marshaller entities
  in
    Expr.insertExpr
      (tableName tableDef)
      (Just insertColumnList)
      insertSource
      (mkTableReturningClause returningOption tableDef)

{- |
  Builds an 'Expr.InsertColumnList' that specifies the columns for an
  insert statement in the order that they appear in the given 'SqlMarshaller'.

  In normal circumstances you will want to build the complete insert statement
  via 'mkInsertExpr', but this is exported in case you are a composing SQL
  yourself and need the column list of an insert as a fragment.
-}
mkInsertColumnList ::
  SqlMarshaller writeEntity readEntity ->
  Expr.InsertColumnList
mkInsertColumnList marshaller =
  Expr.insertColumnList $
    foldMarshallerFields marshaller [] (collectFromField ExcludeReadOnlyColumns fieldColumnName)

{- |
  Builds an 'Expr.InsertSource' that will insert the given entities with their
  values specified in the order that the fields appear in the given
  'SqlMarshaller' (which matches the order of column names produced by
  'mkInsertColumnColumnsList').

  In normal circumstances you will want to build the complete insert statement
  via 'mkInsertExpr', but this is exported in case you are a composing SQL
  yourself and need the column list of an insert as a fragment.
-}
mkInsertSource ::
  SqlMarshaller writeEntity readEntity ->
  NonEmpty writeEntity ->
  Expr.InsertSource
mkInsertSource marshaller entities =
  let
    encodeRow =
      foldMarshallerFields marshaller (const []) collectSqlValue
  in
    Expr.insertSqlValues $ map encodeRow (toList entities)

{- |
  An internal helper function that collects the 'SqlValue' encoded value for a
  field from a Haskell entity, adding it a list of 'SqlValue's that is being
  built.
-}
collectSqlValue ::
  MarshallerField entity ->
  (entity -> [SqlValue]) ->
  entity ->
  [SqlValue]
collectSqlValue entry encodeRest entity =
  case entry of
    Natural fieldDef (Just accessor) ->
      fieldValueToSqlValue fieldDef (accessor entity) : (encodeRest entity)
    Natural _ Nothing ->
      encodeRest entity
    Synthetic _ ->
      encodeRest entity
