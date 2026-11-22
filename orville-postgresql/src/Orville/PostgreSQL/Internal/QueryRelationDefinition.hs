{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

The representation shared by the definitions of relations that are defined
by a query: views and materialized views. The public definition types wrap
it so that they remain distinct kinds of definition while sharing the
behaviour that does not depend on the kind.

@since 1.2.0.0
-}
module Orville.PostgreSQL.Internal.QueryRelationDefinition
  ( QueryRelationDefinition
  , mkQueryRelationDefinition
  , readOnlyTableToQueryRelationDefinition
  , queryRelationIdentifier
  , queryRelationQuery
  , queryRelationMarshaller
  , queryRelationName
  , queryRelationColumnNames
  , setQueryRelationSchema
  , mapQueryRelationMarshaller
  )
where

import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.FieldDefinition (fieldNameToColumnName)
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, MarshallerField (Natural, Synthetic), SqlMarshaller, foldMarshallerFields, mapSqlMarshaller, marshallReadOnly, marshallerDerivedColumns, unannotatedSqlMarshaller)
import Orville.PostgreSQL.Marshall.SyntheticField (syntheticFieldName)
import Orville.PostgreSQL.Schema.ReadOnlyTableDefinition (ReadOnlyTableDefinition, readOnlyTableDefinitionMarshaller, readOnlyTableDefinitionReference)
import Orville.PostgreSQL.Schema.TableIdentifier (TableIdentifier, setTableIdSchema, tableIdQualifiedName, unqualifiedNameToTableId)

{- | A named relation defined by a query, with the marshaller that decodes
  rows selected from it.

@since 1.2.0.0
-}
data QueryRelationDefinition readEntity = QueryRelationDefinition
  { queryRelationIdentifier :: TableIdentifier
  -- ^ The name of the relation and, optionally, the schema it belongs to.
  , queryRelationQuery :: Expr.QueryExpr
  -- ^ The query that defines the relation.
  , queryRelationMarshaller :: AnnotatedSqlMarshaller () readEntity
  {- ^ The marshaller that decodes rows selected from the relation, by the
  names its fields have in a result set.
  -}
  }

{- | Constructs a 'QueryRelationDefinition' with an unqualified name. Any
  write fields of the marshaller are ignored.

@since 1.2.0.0
-}
mkQueryRelationDefinition ::
  String ->
  Expr.QueryExpr ->
  AnnotatedSqlMarshaller writeEntity readEntity ->
  QueryRelationDefinition readEntity
mkQueryRelationDefinition name query marshaller =
  QueryRelationDefinition
    { queryRelationIdentifier = unqualifiedNameToTableId name
    , queryRelationQuery = query
    , queryRelationMarshaller = mapSqlMarshaller marshallReadOnly marshaller
    }

{- | Constructs a 'QueryRelationDefinition' whose query selects the columns
  described by the marshaller of the 'ReadOnlyTableDefinition' from its table
  reference, using the same code path as
  'Orville.PostgreSQL.Execution.selectTable' with empty
  'SelectOptions.SelectOptions'.

@since 1.2.0.0
-}
readOnlyTableToQueryRelationDefinition ::
  String ->
  ReadOnlyTableDefinition readEntity ->
  QueryRelationDefinition readEntity
readOnlyTableToQueryRelationDefinition name readOnlyTable =
  let
    marshaller =
      readOnlyTableDefinitionMarshaller readOnlyTable

    query =
      SelectOptions.selectOptionsQueryExpr
        (Expr.selectDerivedColumns (marshallerDerivedColumns (unannotatedSqlMarshaller marshaller)))
        (Expr.tableReferenceList [readOnlyTableDefinitionReference readOnlyTable])
        SelectOptions.emptySelectOptions
  in
    mkQueryRelationDefinition name query marshaller

{- | The name to refer to the relation by in SQL, qualified by its schema
  when it has one.

@since 1.2.0.0
-}
queryRelationName :: QueryRelationDefinition readEntity -> Expr.QualifiedOrUnqualified Expr.TableName
queryRelationName =
  tableIdQualifiedName . queryRelationIdentifier

{- | The names of the relation's columns in the order the marshaller decodes
  them: the names the marshaller's fields have in a result set.

@since 1.2.0.0
-}
queryRelationColumnNames :: QueryRelationDefinition readEntity -> [Expr.ColumnName]
queryRelationColumnNames relationDef =
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
      (unannotatedSqlMarshaller (queryRelationMarshaller relationDef))
      []
      collectColumnName

{- | Sets the schema that qualifies the relation's name.

@since 1.2.0.0
-}
setQueryRelationSchema ::
  String ->
  QueryRelationDefinition readEntity ->
  QueryRelationDefinition readEntity
setQueryRelationSchema schemaName relationDef =
  relationDef
    { queryRelationIdentifier = setTableIdSchema schemaName (queryRelationIdentifier relationDef)
    }

{- | Applies the provided function to the underlying 'SqlMarshaller'. The
  identifier and query are unchanged.

@since 1.2.0.0
-}
mapQueryRelationMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  QueryRelationDefinition readEntityA ->
  QueryRelationDefinition readEntityB
mapQueryRelationMarshaller f relationDef =
  relationDef
    { queryRelationMarshaller =
        mapSqlMarshaller f (queryRelationMarshaller relationDef)
    }
