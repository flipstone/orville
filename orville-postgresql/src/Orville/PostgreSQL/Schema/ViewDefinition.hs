{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.ViewDefinition
  ( ViewDefinition
  , mkViewDefinition
  , readOnlyTableToViewDefinition
  , viewDefinitionIdentifier
  , viewDefinitionQuery
  , viewDefinitionMarshaller
  , viewDefinitionName
  , viewDefinitionColumnNames
  , setViewSchema
  , mapViewMarshaller
  , mkCreateViewExpr
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.QueryRelationDefinition as QueryRelation
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, SqlMarshaller)
import Orville.PostgreSQL.Schema.ReadOnlyTableDefinition (ReadOnlyTableDefinition)
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier)

{- | The definition of a SQL view: a named query stored in the database whose
  result set can be selected from like a table. Unlike a
  'Orville.PostgreSQL.TableDefinition', a view cannot be inserted into,
  updated or deleted from through Orville, and unlike a
  'ReadOnlyTableDefinition' it is a schema object that auto-migration creates
  and drops (see 'Orville.PostgreSQL.AutoMigration.SchemaView'). A view is
  computed from its query every time it is selected from; see
  'Orville.PostgreSQL.Schema.MaterializedViewDefinition' for a view whose
  result set is stored.

  Rows are selected from a view by the columns the marshaller decodes, which
  are the result columns of the defining query. A 'ViewDefinition' is an
  instance of 'Orville.PostgreSQL.Execution.Selectable', so it can be queried
  with 'Orville.PostgreSQL.Execution.findSelectableEntitiesBy', and it can
  take part in joins via
  'Orville.PostgreSQL.Schema.viewReferenceDefinition'.

@since 1.2.0.0
-}
newtype ViewDefinition readEntity
  = ViewDefinition (QueryRelation.QueryRelationDefinition readEntity)

{- | Constructs a 'ViewDefinition' from the view's name, the query that defines
  it and the marshaller that decodes its rows. The view's columns are the
  result columns of the query, so the marshaller must decode the result set
  of the query. Any write fields of the marshaller are ignored.

  The view will not be qualified by a schema unless 'setViewSchema' is
  called afterwards.

@since 1.2.0.0
-}
mkViewDefinition ::
  -- | The name of the view, treated as a SQL identifier.
  String ->
  -- | The query that defines the view.
  Expr.QueryExpr ->
  -- | The marshaller that decodes the result set of the query.
  AnnotatedSqlMarshaller writeEntity readEntity ->
  ViewDefinition readEntity
mkViewDefinition name query marshaller =
  ViewDefinition (QueryRelation.mkQueryRelationDefinition name query marshaller)

{- | Constructs a 'ViewDefinition' whose query selects the columns described by
  the marshaller of the 'ReadOnlyTableDefinition' from its table reference, as
  'Orville.PostgreSQL.Execution.selectTable' would with empty
  'Orville.PostgreSQL.SelectOptions'. The marshaller of the
  'ReadOnlyTableDefinition' becomes the marshaller of the view, so a join
  described as a 'ReadOnlyTableDefinition' can be stored as a view and
  selected from without describing its columns twice.

  The view will not be qualified by a schema unless 'setViewSchema' is
  called afterwards.

@since 1.2.0.0
-}
readOnlyTableToViewDefinition ::
  -- | The name of the view, treated as a SQL identifier.
  String ->
  ReadOnlyTableDefinition readEntity ->
  ViewDefinition readEntity
readOnlyTableToViewDefinition name readOnlyTable =
  ViewDefinition (QueryRelation.readOnlyTableToQueryRelationDefinition name readOnlyTable)

{- | The name of the view and, optionally, the schema it belongs to. A view
  is identified in the same way as a table because it is a relation in the
  same namespace as tables.

@since 1.2.0.0
-}
viewDefinitionIdentifier :: ViewDefinition readEntity -> TableIdentifier
viewDefinitionIdentifier (ViewDefinition relationDef) =
  QueryRelation.queryRelationIdentifier relationDef

{- | The query that defines the view, used in @CREATE VIEW ... AS@.

@since 1.2.0.0
-}
viewDefinitionQuery :: ViewDefinition readEntity -> Expr.QueryExpr
viewDefinitionQuery (ViewDefinition relationDef) =
  QueryRelation.queryRelationQuery relationDef

{- | The marshaller that decodes rows selected from the view. Its fields are
  decoded by the names they have in a result set, which are the view's column
  names. Nothing is ever written through a 'ViewDefinition', so the marshaller
  has no write entity.

@since 1.2.0.0
-}
viewDefinitionMarshaller :: ViewDefinition readEntity -> AnnotatedSqlMarshaller () readEntity
viewDefinitionMarshaller (ViewDefinition relationDef) =
  QueryRelation.queryRelationMarshaller relationDef

{- | Retrieves the 'Expr.QualifiedOrUnqualified' 'Expr.TableName' that should be
  used to refer to the view in SQL expressions. It is qualified by the schema
  name when one has been set with 'setViewSchema'.

@since 1.2.0.0
-}
viewDefinitionName :: ViewDefinition readEntity -> Expr.QualifiedOrUnqualified Expr.TableName
viewDefinitionName (ViewDefinition relationDef) =
  QueryRelation.queryRelationName relationDef

{- | The names of the view's columns, in the order the marshaller decodes
  them. These are the names the marshaller's fields have in a result set:
  the field's own name, or the prefixed name of a field aliased with
  'Orville.PostgreSQL.marshallAliasFields', or the name of a synthetic
  field.

@since 1.2.0.0
-}
viewDefinitionColumnNames :: ViewDefinition readEntity -> [Expr.ColumnName]
viewDefinitionColumnNames (ViewDefinition relationDef) =
  QueryRelation.queryRelationColumnNames relationDef

{- | Sets the view's schema to the name in the given 'String', which will be
  treated as a SQL identifier. If a view has a schema name set, it will be
  included as a qualifier on the view name for all queries involving the
  view.

@since 1.2.0.0
-}
setViewSchema ::
  String ->
  ViewDefinition readEntity ->
  ViewDefinition readEntity
setViewSchema schemaName (ViewDefinition relationDef) =
  ViewDefinition (QueryRelation.setQueryRelationSchema schemaName relationDef)

{- | Applies the provided function to the underlying 'SqlMarshaller' of the
  'ViewDefinition', for instance to reshape the decoded entity with @fmap@.
  The identifier and query are unchanged.

@since 1.2.0.0
-}
mapViewMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  ViewDefinition readEntityA ->
  ViewDefinition readEntityB
mapViewMarshaller f (ViewDefinition relationDef) =
  ViewDefinition (QueryRelation.mapQueryRelationMarshaller f relationDef)

{- | Builds a 'Expr.CreateViewExpr' that will create a SQL view matching the
  given 'ViewDefinition' when it is executed.

@since 1.2.0.0
-}
mkCreateViewExpr ::
  ViewDefinition readEntity ->
  -- | An optional @OR REPLACE@, to replace an existing view of the same name.
  Maybe Expr.OrReplace ->
  Expr.CreateViewExpr
mkCreateViewExpr viewDef maybeOrReplace =
  Expr.createViewExpr
    maybeOrReplace
    Nothing
    Nothing
    (viewDefinitionName viewDef)
    Nothing
    []
    (viewDefinitionQuery viewDef)
    Nothing
