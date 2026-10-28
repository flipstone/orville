{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.MaterializedViewDefinition
  ( MaterializedViewDefinition
  , mkMaterializedViewDefinition
  , readOnlyTableToMaterializedViewDefinition
  , materializedViewDefinitionIdentifier
  , materializedViewDefinitionQuery
  , materializedViewDefinitionMarshaller
  , materializedViewDefinitionName
  , materializedViewDefinitionColumnNames
  , setMaterializedViewSchema
  , mapMaterializedViewMarshaller
  , mkCreateMaterializedViewExpr
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.QueryRelationDefinition as QueryRelation
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, SqlMarshaller)
import Orville.PostgreSQL.Schema.ReadOnlyTableDefinition (ReadOnlyTableDefinition)
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier)

{- | The definition of a SQL materialized view: a named query whose result set
  is stored in the database and can be selected from like a table. Like a
  'Orville.PostgreSQL.Schema.ViewDefinition', a materialized view cannot be
  inserted into, updated or deleted from through Orville, and it is a schema
  object that auto-migration creates and drops (see
  'Orville.PostgreSQL.AutoMigration.SchemaMaterializedView'). Unlike a view,
  its stored rows only change when it is refreshed with
  'Orville.PostgreSQL.Execution.refreshMaterializedView'.

  Rows are selected from a materialized view by the columns the marshaller
  decodes, which are the result columns of the defining query. A
  'MaterializedViewDefinition' is an instance of
  'Orville.PostgreSQL.Execution.Selectable', so it can be queried with
  'Orville.PostgreSQL.Execution.findSelectableEntitiesBy', and it can take
  part in joins via
  'Orville.PostgreSQL.Schema.materializedViewReferenceDefinition'.

@since 1.2.0.0
-}
newtype MaterializedViewDefinition readEntity
  = MaterializedViewDefinition (QueryRelation.QueryRelationDefinition readEntity)

{- | Constructs a 'MaterializedViewDefinition' from the materialized view's
  name, the query that defines it and the marshaller that decodes its rows.
  The materialized view's columns are the result columns of the query, so
  the marshaller must decode the result set of the query. Any write fields of
  the marshaller are ignored.

  The materialized view will not be qualified by a schema unless
  'setMaterializedViewSchema' is called afterwards.

@since 1.2.0.0
-}
mkMaterializedViewDefinition ::
  -- | The name of the materialized view, treated as a SQL identifier.
  String ->
  -- | The query that defines the materialized view.
  Expr.QueryExpr ->
  -- | The marshaller that decodes the result set of the query.
  AnnotatedSqlMarshaller writeEntity readEntity ->
  MaterializedViewDefinition readEntity
mkMaterializedViewDefinition name query marshaller =
  MaterializedViewDefinition (QueryRelation.mkQueryRelationDefinition name query marshaller)

{- | Constructs a 'MaterializedViewDefinition' whose query selects the columns
  described by the marshaller of the 'ReadOnlyTableDefinition' from its table
  reference, as 'Orville.PostgreSQL.Execution.selectTable' would with empty
  'Orville.PostgreSQL.SelectOptions'. The marshaller of the
  'ReadOnlyTableDefinition' becomes the marshaller of the materialized view,
  so a join described as a 'ReadOnlyTableDefinition' can be stored as a
  materialized view and selected from without describing its columns twice.

  The materialized view will not be qualified by a schema unless
  'setMaterializedViewSchema' is called afterwards.

@since 1.2.0.0
-}
readOnlyTableToMaterializedViewDefinition ::
  -- | The name of the materialized view, treated as a SQL identifier.
  String ->
  ReadOnlyTableDefinition readEntity ->
  MaterializedViewDefinition readEntity
readOnlyTableToMaterializedViewDefinition name readOnlyTable =
  MaterializedViewDefinition (QueryRelation.readOnlyTableToQueryRelationDefinition name readOnlyTable)

{- | The name of the materialized view and, optionally, the schema it belongs
  to. A materialized view is identified in the same way as a table because it
  is a relation in the same namespace as tables.

@since 1.2.0.0
-}
materializedViewDefinitionIdentifier :: MaterializedViewDefinition readEntity -> TableIdentifier
materializedViewDefinitionIdentifier (MaterializedViewDefinition relationDef) =
  QueryRelation.queryRelationIdentifier relationDef

{- | The query that defines the materialized view, used in
  @CREATE MATERIALIZED VIEW ... AS@.

@since 1.2.0.0
-}
materializedViewDefinitionQuery :: MaterializedViewDefinition readEntity -> Expr.QueryExpr
materializedViewDefinitionQuery (MaterializedViewDefinition relationDef) =
  QueryRelation.queryRelationQuery relationDef

{- | The marshaller that decodes rows selected from the materialized view. Its
  fields are decoded by the names they have in a result set, which are the
  materialized view's column names. Nothing is ever written through a
  'MaterializedViewDefinition', so the marshaller has no write entity.

@since 1.2.0.0
-}
materializedViewDefinitionMarshaller :: MaterializedViewDefinition readEntity -> AnnotatedSqlMarshaller () readEntity
materializedViewDefinitionMarshaller (MaterializedViewDefinition relationDef) =
  QueryRelation.queryRelationMarshaller relationDef

{- | Retrieves the 'Expr.QualifiedOrUnqualified' 'Expr.TableName' that should be
  used to refer to the materialized view in SQL expressions. It is qualified
  by the schema name when one has been set with 'setMaterializedViewSchema'.

@since 1.2.0.0
-}
materializedViewDefinitionName :: MaterializedViewDefinition readEntity -> Expr.QualifiedOrUnqualified Expr.TableName
materializedViewDefinitionName (MaterializedViewDefinition relationDef) =
  QueryRelation.queryRelationName relationDef

{- | The names of the materialized view's columns, in the order the
  marshaller decodes them. These are the names the marshaller's fields have
  in a result set: the field's own name, or the prefixed name of a field
  aliased with 'Orville.PostgreSQL.marshallAliasFields', or the name of a
  synthetic field.

@since 1.2.0.0
-}
materializedViewDefinitionColumnNames :: MaterializedViewDefinition readEntity -> [Expr.ColumnName]
materializedViewDefinitionColumnNames (MaterializedViewDefinition relationDef) =
  QueryRelation.queryRelationColumnNames relationDef

{- | Sets the materialized view's schema to the name in the given 'String',
  which will be treated as a SQL identifier. If a materialized view has a
  schema name set, it will be included as a qualifier on the materialized
  view name for all queries involving it.

@since 1.2.0.0
-}
setMaterializedViewSchema ::
  String ->
  MaterializedViewDefinition readEntity ->
  MaterializedViewDefinition readEntity
setMaterializedViewSchema schemaName (MaterializedViewDefinition relationDef) =
  MaterializedViewDefinition (QueryRelation.setQueryRelationSchema schemaName relationDef)

{- | Applies the provided function to the underlying 'SqlMarshaller' of the
  'MaterializedViewDefinition', for instance to reshape the decoded entity
  with @fmap@. The identifier and query are unchanged.

@since 1.2.0.0
-}
mapMaterializedViewMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  MaterializedViewDefinition readEntityA ->
  MaterializedViewDefinition readEntityB
mapMaterializedViewMarshaller f (MaterializedViewDefinition relationDef) =
  MaterializedViewDefinition (QueryRelation.mapQueryRelationMarshaller f relationDef)

{- | Builds a 'Expr.CreateMaterializedViewExpr' that will create a SQL
  materialized view matching the given 'MaterializedViewDefinition' when it
  is executed.

@since 1.2.0.0
-}
mkCreateMaterializedViewExpr ::
  MaterializedViewDefinition readEntity ->
  {- | An optional @WITH DATA@ or @WITH NO DATA@. PostgreSQL populates the
  materialized view when the clause is omitted.
  -}
  Maybe Expr.WithDataExpr ->
  Expr.CreateMaterializedViewExpr
mkCreateMaterializedViewExpr viewDef maybeWithData =
  Expr.createMaterializedViewExpr
    Nothing
    (materializedViewDefinitionName viewDef)
    Nothing
    Nothing
    []
    Nothing
    (materializedViewDefinitionQuery viewDef)
    maybeWithData
