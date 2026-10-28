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

import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.FieldDefinition (fieldNameToColumnName)
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, MarshallerField (Natural, Synthetic), SqlMarshaller, foldMarshallerFields, mapSqlMarshaller, marshallReadOnly, marshallerDerivedColumns, unannotatedSqlMarshaller)
import Orville.PostgreSQL.Marshall.SyntheticField (syntheticFieldName)
import Orville.PostgreSQL.Schema.ReadOnlyTableDefinition (ReadOnlyTableDefinition, readOnlyTableDefinitionMarshaller, readOnlyTableDefinitionReference)
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier, setTableIdSchema, tableIdQualifiedName, unqualifiedNameToTableId)

{- | The definition of a SQL view: a named query stored in the database whose
  result set can be selected from like a table. Unlike a
  'Orville.PostgreSQL.TableDefinition', a view cannot be inserted into,
  updated or deleted from through Orville, and unlike a
  'ReadOnlyTableDefinition' it is a schema object that auto-migration creates
  and drops (see 'Orville.PostgreSQL.AutoMigration.SchemaView').

  Rows are selected from a view by the columns the marshaller decodes, which
  are the result columns of the defining query. A 'ViewDefinition' is an
  instance of 'Orville.PostgreSQL.Execution.Selectable', so it can be queried
  with 'Orville.PostgreSQL.Execution.findSelectableEntitiesBy', and it can
  take part in joins via
  'Orville.PostgreSQL.Schema.viewReferenceDefinition'.

@since 1.2.0.0
-}
data ViewDefinition readEntity = ViewDefinition
  { viewDefinitionIdentifier :: TableIdentifier
  {- ^ The name of the view and, optionally, the schema it belongs to. A view
  is identified in the same way as a table because it is a relation in the
  same namespace as tables.

  @since 1.2.0.0
  -}
  , viewDefinitionQuery :: Expr.QueryExpr
  {- ^ The query that defines the view, used in @CREATE VIEW ... AS@.

  @since 1.2.0.0
  -}
  , viewDefinitionMarshaller :: AnnotatedSqlMarshaller () readEntity
  {- ^ The marshaller that decodes rows selected from the view. Its fields are
  decoded by the names they have in a result set, which are the view's column
  names. Nothing is ever written through a 'ViewDefinition', so the marshaller
  has no write entity.

  @since 1.2.0.0
  -}
  }

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
  ViewDefinition
    { viewDefinitionIdentifier = unqualifiedNameToTableId name
    , viewDefinitionQuery = query
    , viewDefinitionMarshaller = mapSqlMarshaller marshallReadOnly marshaller
    }

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
  let
    marshaller =
      readOnlyTableDefinitionMarshaller readOnlyTable

    query =
      SelectOptions.selectOptionsQueryExpr
        (Expr.selectDerivedColumns (marshallerDerivedColumns (unannotatedSqlMarshaller marshaller)))
        (Expr.tableReferenceList [readOnlyTableDefinitionReference readOnlyTable])
        SelectOptions.emptySelectOptions
  in
    mkViewDefinition name query marshaller

{- | Retrieves the 'Expr.QualifiedOrUnqualified' 'Expr.TableName' that should be
  used to refer to the view in SQL expressions. It is qualified by the schema
  name when one has been set with 'setViewSchema'.

@since 1.2.0.0
-}
viewDefinitionName :: ViewDefinition readEntity -> Expr.QualifiedOrUnqualified Expr.TableName
viewDefinitionName =
  tableIdQualifiedName . viewDefinitionIdentifier

{- | The names of the view's columns, in the order the marshaller decodes
  them. These are the names the marshaller's fields have in a result set:
  the field's own name, or the prefixed name of a field aliased with
  'Orville.PostgreSQL.marshallAliasFields', or the name of a synthetic
  field.

@since 1.2.0.0
-}
viewDefinitionColumnNames :: ViewDefinition readEntity -> [Expr.ColumnName]
viewDefinitionColumnNames viewDef =
  let
    collectColumnName :: MarshallerField () -> [Expr.ColumnName] -> [Expr.ColumnName]
    collectColumnName entry columns =
      case entry of
        Natural _ _ resultName _ ->
          fieldNameToColumnName resultName : columns
        Synthetic synthField ->
          fieldNameToColumnName (syntheticFieldName synthField) : columns
  in
    foldMarshallerFields
      (unannotatedSqlMarshaller (viewDefinitionMarshaller viewDef))
      []
      collectColumnName

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
setViewSchema schemaName viewDef =
  viewDef
    { viewDefinitionIdentifier = setTableIdSchema schemaName (viewDefinitionIdentifier viewDef)
    }

{- | Applies the provided function to the underlying 'SqlMarshaller' of the
  'ViewDefinition', for instance to reshape the decoded entity with @fmap@.
  The identifier and query are unchanged.

@since 1.2.0.0
-}
mapViewMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  ViewDefinition readEntityA ->
  ViewDefinition readEntityB
mapViewMarshaller f viewDef =
  viewDef
    { viewDefinitionMarshaller =
        mapSqlMarshaller f (viewDefinitionMarshaller viewDef)
    }

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
