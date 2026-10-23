{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.TableReferenceDefinition
  ( TableReferenceDefinition
  , tableReferenceDefinition
  , aliasedTableReferenceDefinition
  , tableReferenceDefinitionReadOnlyTable
  , tableReferenceDefinitionReference
  , tableReferenceDefinitionQualifier
  , tableReferenceDefinitionResultPrefix
  , tableReferenceDefinitionMarshaller
  , mapTableReferenceMarshaller
  , qualifyTableField
  , marshallTableReferenceFields
  , tableReferenceQualifiedMarshaller
  , tableReferenceRowIdFieldNames
  , joinTables
  , joinOnFieldsEqual
  , innerJoinOnFieldsEqual
  , leftJoinOnFieldsEqual
  , joinOnCondition
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall.AliasName as AliasName
import Orville.PostgreSQL.Marshall.FieldDefinition (FieldDefinition, FieldName, FieldQualifier, QualifiedFieldDefinition, SomeFieldQualifier (SomeFieldQualifier), qualifyField)
import qualified Orville.PostgreSQL.Marshall.SqlComparable as SqlComparable
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, SqlMarshaller, marshallAliasFields, marshallQualifyFields, marshallReadOnly, rowIdFieldNames, unannotatedSqlMarshaller)
import Orville.PostgreSQL.Schema.ReadOnlyTableDefinition (ReadOnlyTableDefinition, mapReadOnlyTableMarshaller, mkReadOnlyTableDefinition, readOnlyTableDefinitionMarshaller, readOnlyTableDefinitionReference)
import Orville.PostgreSQL.Schema.TableDefinition (TableDefinition, tableIdentifier, tableMarshaller, tableName)
import qualified Orville.PostgreSQL.Schema.TableIdentifier as TableIdentifier

{- | Contains the definition of a SQL item, such as a table, for Orville to use generating queries
   that involve one or more such items. For example this is the building block for creating queries
   that use @JOIN@.

  * @readEntity@ is the Haskell type for values that Orville will decode
    from the result set when entities are queried.

  See 'tableReferenceDefinition' and 'aliasedTableReferenceDefinition' for constructing values of
  this type.

@since 1.2.0.0
-}
data TableReferenceDefinition readEntity = TableReferenceDefinition
  { i_tableReferenceDefinitionReadOnlyTable :: ReadOnlyTableDefinition readEntity
  , i_tableReferenceDefinitionQualifier :: SomeFieldQualifier
  , i_tableReferenceDefinitionResultPrefix :: AliasName.AliasName
  }

{- | The 'ReadOnlyTableDefinition' that the 'TableReferenceDefinition' is built on: the reference for
the @FROM@ clause together with the marshaller for the item's columns, without the name the item
exposes for qualifying its columns. Selecting from it produces the same query as selecting from the
item it was built from, with the table reference (including any alias) in the @FROM@ clause.

@since 1.2.0.0
-}
tableReferenceDefinitionReadOnlyTable :: TableReferenceDefinition readEntity -> ReadOnlyTableDefinition readEntity
tableReferenceDefinitionReadOnlyTable = i_tableReferenceDefinitionReadOnlyTable

{- | The reference to the table for use in a @FROM@ clause. For example,
@"book"@ or @"book" AS "b"@.

@since 1.2.0.0
-}
tableReferenceDefinitionReference :: TableReferenceDefinition readEntity -> Expr.TableReference
tableReferenceDefinitionReference =
  readOnlyTableDefinitionReference . i_tableReferenceDefinitionReadOnlyTable

{- | The qualifier used wherever the table's columns are referenced in the
query, such as in @WHERE@ and @ON@ conditions.

@since 1.2.0.0
-}
tableReferenceDefinitionQualifier :: TableReferenceDefinition readEntity -> SomeFieldQualifier
tableReferenceDefinitionQualifier = i_tableReferenceDefinitionQualifier

{- | The prefix given to the table's columns in the result set so they cannot collide with columns
other parts of the query. See 'AliasName.aliasNameAndFieldNameToResultFieldName'.

@since 1.2.0.0
-}
tableReferenceDefinitionResultPrefix :: TableReferenceDefinition readEntity -> AliasName.AliasName
tableReferenceDefinitionResultPrefix = i_tableReferenceDefinitionResultPrefix

{- | The marshaller for the items's columns, read-only and with its fields not yet qualified. See
'tableReferenceQualifiedMarshaller' for the marshaller to use when decoding the results of a query
that involves more than one table.

@since 1.2.0.0
-}
tableReferenceDefinitionMarshaller :: TableReferenceDefinition readEntity -> AnnotatedSqlMarshaller () readEntity
tableReferenceDefinitionMarshaller =
  readOnlyTableDefinitionMarshaller . i_tableReferenceDefinitionReadOnlyTable

{- | Applies the provided function to the underlying 'SqlMarshaller' of the
  'TableReferenceDefinition'. The table reference, qualifier and result prefix are unchanged.

@since 1.2.0.0
-}
mapTableReferenceMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  TableReferenceDefinition readEntityA ->
  TableReferenceDefinition readEntityB
mapTableReferenceMarshaller f tableRef =
  tableRef
    { i_tableReferenceDefinitionReadOnlyTable =
        mapReadOnlyTableMarshaller f (i_tableReferenceDefinitionReadOnlyTable tableRef)
    }

{- | Builds a 'TableReferenceDefinition' from a 'TableDefinition' using its own name. Column
  references are qualified by the table's 'TableIdentifier.TableIdentifier', so they include the
  schema name when the table has one. Result columns are prefixed with the table name, preceded by
  the schema name when there is one, as in @"my_schema"."book"."id" AS "my_schema_book_id"@.

  Two tables with the same name in different schemas can therefore be joined this way. This is not
  suitable to join a table to itself, for that at least one side should use
  'aliasedTableReferenceDefinition'.

@since 1.2.0.0
-}
tableReferenceDefinition ::
  TableDefinition key writeEntity readEntity ->
  TableReferenceDefinition readEntity
tableReferenceDefinition tableDef =
  let
    tableId =
      tableIdentifier tableDef

    unqualifiedName =
      TableIdentifier.tableIdUnqualifiedNameString tableId

    resultPrefix =
      case TableIdentifier.tableIdSchemaNameString tableId of
        Nothing -> unqualifiedName
        Just schemaName -> schemaName <> "_" <> unqualifiedName
  in
    TableReferenceDefinition
      { i_tableReferenceDefinitionReadOnlyTable =
          mkReadOnlyTableDefinition
            (Expr.tableNameReference (tableName tableDef) Nothing)
            (tableMarshaller tableDef)
      , i_tableReferenceDefinitionQualifier = SomeFieldQualifier tableId
      , i_tableReferenceDefinitionResultPrefix = AliasName.stringToAliasName resultPrefix
      }

{- | Builds a 'TableReferenceDefinition' for a 'TableDefinition' where the it will be refered to by an
  alias, as in @"book" AS "b"@. Column references are qualified by the alias and result columns are
  prefixed with it. An alias is needed when the same table appears more than once in a query, and
  may be used otherwise for brevity.

@since 1.2.0.0
-}
aliasedTableReferenceDefinition ::
  AliasName.AliasName ->
  TableDefinition key writeEntity readEntity ->
  TableReferenceDefinition readEntity
aliasedTableReferenceDefinition alias tableDef =
  TableReferenceDefinition
    { i_tableReferenceDefinitionReadOnlyTable =
        mkReadOnlyTableDefinition
          (Expr.tableNameReference (tableName tableDef) (Just (AliasName.aliasNameToAliasExpr alias)))
          (tableMarshaller tableDef)
    , i_tableReferenceDefinitionQualifier = SomeFieldQualifier alias
    , i_tableReferenceDefinitionResultPrefix = alias
    }

{- | Qualifies a field of the table with the table reference's qualifier, for use in conditions and
  ordering via the 'SqlComparable.SqlComparable' functions such as 'SqlComparable.equals' and
  'SqlComparable.orderBySqlComparable'. Note that the field is not checked to belong to the table.

@since 1.2.0.0
-}
qualifyTableField ::
  TableReferenceDefinition readEntity ->
  FieldDefinition nullability a ->
  QualifiedFieldDefinition SomeFieldQualifier nullability a
qualifyTableField tableRef =
  qualifyField (tableReferenceDefinitionQualifier tableRef)

{- | Qualifies the field references of a marshaller with the table reference's qualifier and prefixes
  its result columns with the table reference's result prefix, so that the marshaller can be
  combined with the marshallers of other tables to decode the result of a join. The marshaller
  should contain only fields of the table, but may contain any subset of them.

  The query the resulting marshaller is used with must include the table's
  'tableReferenceDefinitionReference' in its @FROM@ clause.

@since 1.2.0.0
-}
marshallTableReferenceFields ::
  TableReferenceDefinition readEntity ->
  SqlMarshaller writeEntity a ->
  SqlMarshaller writeEntity a
marshallTableReferenceFields tableRef =
  marshallAliasFields (tableReferenceDefinitionResultPrefix tableRef)
    . marshallQualifyFields (tableReferenceDefinitionQualifier tableRef)

{- | The table's whole marshaller qualified and prefixed via 'marshallTableReferenceFields', for
  decoding all of the table's columns from the result of a join.

  The marshaller is read-only, so it has no write fields to compare: do not use it with the
  'SqlComparable.SqlComparable' functions. Use 'qualifyTableField' to refer to the table's columns
  in conditions.

@since 1.2.0.0
-}
tableReferenceQualifiedMarshaller ::
  TableReferenceDefinition readEntity ->
  SqlMarshaller writeEntity readEntity
tableReferenceQualifiedMarshaller tableRef =
  marshallReadOnly
    . marshallTableReferenceFields tableRef
    . unannotatedSqlMarshaller
    $ tableReferenceDefinitionMarshaller tableRef

{- | The names the items's row-identifying columns (often its primary key in the case of a single
  table) will have in a result set when selected via 'tableReferenceQualifiedMarshaller'. These can
  be passed to 'Orville.PostgreSQL.annotateSqlMarshaller' so that decoding errors identify the row
  they occurred on.

@since 1.2.0.0
-}
tableReferenceRowIdFieldNames ::
  TableReferenceDefinition readEntity ->
  [FieldName]
tableReferenceRowIdFieldNames tableRef =
  fmap
    (AliasName.aliasNameAndFieldNameToResultFieldName (tableReferenceDefinitionResultPrefix tableRef))
    (rowIdFieldNames (tableReferenceDefinitionMarshaller tableRef))

{- | Builds the 'Expr.TableReference' for a @FROM@ clause that starts from the given table and joins
  the others onto it in order.

  @
  joinTables book
    [ innerJoinOnFieldsEqual author authorIdField (qualifyTableField book bookAuthorIdField)
    , leftJoinOnFieldsEqual publisher publisherIdField (qualifyTableField author authorPublisherIdField)
    ]
  @

@since 1.2.0.0
-}
joinTables ::
  TableReferenceDefinition readEntity ->
  [Expr.TableReference -> Expr.TableReference] ->
  Expr.TableReference
joinTables tableRef =
  Expr.joining (tableReferenceDefinitionReference tableRef)

{- | Builds a 'Expr.TableReference' that joins an item, such as table or a tables that are already
  joined, onto an existing table reference where one of its columns equals a column of some item
  already in the reference. The joined item's column is qualified with the joined item's qualifier,
  and both columns must have the same Haskell type.

  See also 'joinOnCondition' if you need lower level control of the @ON@.

@since 1.2.0.0
-}
joinOnFieldsEqual ::
  FieldQualifier qualifier =>
  Expr.JoinType ->
  -- | The table being joined
  TableReferenceDefinition readEntity ->
  -- | The column of the table being joined
  FieldDefinition nullability a ->
  -- | The column of a table already in the table reference
  QualifiedFieldDefinition qualifier existingNullability a ->
  Expr.TableReference ->
  Expr.TableReference
joinOnFieldsEqual joinType joined joinedField existingField =
  joinOnCondition joinType joined $
    Expr.equals
      (SqlComparable.referenceValueExpression (qualifyTableField joined joinedField))
      (SqlComparable.referenceValueExpression existingField)

{- | A helper of 'joinOnFieldsEqual' for an @INNER@ join.

@since 1.2.0.0
-}
innerJoinOnFieldsEqual ::
  FieldQualifier qualifier =>
  TableReferenceDefinition readEntity ->
  FieldDefinition nullability a ->
  QualifiedFieldDefinition qualifier existingNullability a ->
  Expr.TableReference ->
  Expr.TableReference
innerJoinOnFieldsEqual =
  joinOnFieldsEqual Expr.innerJoinType

{- | A helper of 'joinOnFieldsEqual' for a @LEFT@ join.

@since 1.2.0.0
-}
leftJoinOnFieldsEqual ::
  FieldQualifier qualifier =>
  TableReferenceDefinition readEntity ->
  FieldDefinition nullability a ->
  QualifiedFieldDefinition qualifier existingNullability a ->
  Expr.TableReference ->
  Expr.TableReference
leftJoinOnFieldsEqual =
  joinOnFieldsEqual Expr.leftJoinType

{- | Joins a table onto a table reference with an arbitrary join condition. Use 'qualifyTableField' to
  refer to the columns of the joined table in the condition. This is the escape hatch for joins that
  'joinOnFieldsEqual' cannot express, such as those on more than one column or with conditions other
  than equality.

@since 1.2.0.0
-}
joinOnCondition ::
  Expr.JoinType ->
  TableReferenceDefinition readEntity ->
  Expr.BooleanExpr ->
  Expr.TableReference ->
  Expr.TableReference
joinOnCondition joinType joined =
  Expr.join joinType (tableReferenceDefinitionReference joined) . Expr.joinOnConstraint
