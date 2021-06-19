{-# LANGUAGE RankNTypes #-}

module Database.Orville.PostgreSQL.Internal.TableDefinition
  ( TableDefinition,
    mkTableDefiniton,
    tableName,
    tablePrimaryKey,
    tableMarshaller,
    mkInsertExpr,
    mkCreateTableExpr,
    mkInsertColumnList,
    mkInsertSource,
    mkQueryExpr,
  )
where

import Data.List.NonEmpty (NonEmpty, toList)

import qualified Database.Orville.PostgreSQL.Internal.Expr as Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, fieldColumnDefinition, fieldColumnName, fieldValueToSqlValue)
import Database.Orville.PostgreSQL.Internal.PrimaryKey (PrimaryKey, mkPrimaryKeyExpr)
import Database.Orville.PostgreSQL.Internal.SqlMarshaller (FieldFold, SqlMarshaller, foldMarshallerFields)
import Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)

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
  { _tableName :: Expr.TableName
  , _tablePrimaryKey :: PrimaryKey key
  , _tableMarshaller :: SqlMarshaller writeEntity readEntity
  }

{- |
  Constructs a new 'TableDefinition' with the minimal fields required for
  operation.
-}
mkTableDefiniton ::
  -- | The name of the table
  String ->
  -- | Definition of the table's primary key
  PrimaryKey key ->
  -- | A 'SqlMarshaller' to marshall table entities to and from the database
  SqlMarshaller writeEntity readEntity ->
  TableDefinition key writeEntity readEntity
mkTableDefiniton name primaryKey marshaller =
  TableDefinition
    { _tableName = Expr.rawTableName name
    , _tablePrimaryKey = primaryKey
    , _tableMarshaller = marshaller
    }

{- |
  Returns the tables name in as an expression that can be used to build SQL
  statements.
-}
tableName :: TableDefinition key writeEntity readEntity -> Expr.TableName
tableName = _tableName

{- |
  Returns the primary key for the table, as defined at construction via 'mkTableDefinition'.
-}
tablePrimaryKey :: TableDefinition key writeEntity readEntity -> PrimaryKey key
tablePrimaryKey = _tablePrimaryKey

{- |
  Returns the marshaller for the table, as defined at construction via 'mkTableDefinition'.
-}
tableMarshaller :: TableDefinition key writeEntity readEntity -> SqlMarshaller writeEntity readEntity
tableMarshaller = _tableMarshaller

{- |
  Builds a 'Expr.CreateTableExpr' that will create a SQL table matching the
  given 'TableDefinition' when it is executed.
-}
mkCreateTableExpr ::
  TableDefinition key writeEntity readEntity ->
  Expr.CreateTableExpr
mkCreateTableExpr tableDef =
  let columnDefinitions =
        foldMarshallerFields
          (tableMarshaller tableDef)
          []
          (collectFromField fieldColumnDefinition)
   in Expr.createTableExpr
        (tableName tableDef)
        columnDefinitions
        (Just . mkPrimaryKeyExpr . tablePrimaryKey $ tableDef)

{- |
  Builds an 'Expr.InsertExpr' that will insert the given entities into the
  SQL table when it is executed.
-}
mkInsertExpr ::
  TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  Expr.InsertExpr
mkInsertExpr tableDef entities =
  let columnList =
        mkInsertColumnList . tableMarshaller $ tableDef

      insertSource =
        mkInsertSource (tableMarshaller tableDef) entities
   in Expr.insertExpr (tableName tableDef) (Just columnList) insertSource

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
mkInsertColumnList =
  Expr.insertColumnList . marshallerColumnNames

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
  let encodeRow =
        foldMarshallerFields marshaller (const []) collectSqlValue
   in Expr.insertSqlValues $ map encodeRow (toList entities)

{- |
  An internal helper function that collects the 'SqlValue' encoded value for a
  field from a Haskell entity, adding it a list of 'SqlValue's that is being
  built.
-}
collectSqlValue :: FieldFold entity (entity -> [SqlValue])
collectSqlValue fieldDef accessor encodeRest entity =
  fieldValueToSqlValue fieldDef (accessor entity) : (encodeRest entity)

{- |
  Builds a 'Expr.QueryExpr' that will do a select from the SQL table described
  by the table definiton, selecting all the columns found in the table's
  'SqlMarshaller'.
-}
mkQueryExpr ::
  TableDefinition key writeEntity readEntity ->
  Maybe Expr.WhereClause ->
  Maybe Expr.OrderByClause ->
  Expr.QueryExpr
mkQueryExpr tableDef whereClause orderByClause =
  let columns =
        marshallerColumnNames . tableMarshaller $ tableDef
   in Expr.queryExpr
        (Expr.selectColumns columns)
        (Expr.tableExpr (tableName tableDef) whereClause orderByClause)

{- |
  An internal helper function that collects the column names for all the
  'FieldDefinition's that are referenced by the given 'SqlMarshaller'.
-}
marshallerColumnNames :: SqlMarshaller writeEntity readEntity -> [Expr.ColumnName]
marshallerColumnNames marshaller =
  foldMarshallerFields marshaller [] (collectFromField fieldColumnName)

{- |
  An internal helper function that collects a value derived from a
  'FieldDefinition' via the given function. The derived value is added to the
  list of values being built.
-}
collectFromField ::
  (forall nullability a. FieldDefinition nullability a -> result) ->
  FieldFold entity [result]
collectFromField fromField fieldDef _ results =
  fromField fieldDef : results
