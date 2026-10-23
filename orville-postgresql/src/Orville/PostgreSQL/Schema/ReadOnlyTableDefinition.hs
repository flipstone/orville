{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Schema.ReadOnlyTableDefinition
  ( ReadOnlyTableDefinition
  , mkReadOnlyTableDefinition
  , readOnlyTableDefinitionReference
  , readOnlyTableDefinitionMarshaller
  , mapReadOnlyTableMarshaller
  )
where

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.SqlMarshaller (AnnotatedSqlMarshaller, SqlMarshaller, mapSqlMarshaller, marshallReadOnly)

{- | A definition of something that rows can be selected from, but not inserted into, updated or
  deleted, and which is not managed by migrations. The 'Expr.TableReference' may be a single table,
  but is intended for more complex cases such as a join of several tables, with the marshaller
  decoding columns from each of them. See "Orville.PostgreSQL.Schema.TableReferenceDefinition" for
  building the two halves from existing table definitions: the table reference with
  'Orville.PostgreSQL.Schema.joinTables' and the marshaller with
  'Orville.PostgreSQL.Schema.marshallTableReferenceFields'.

  * @readEntity@ is the Haskell type for values that Orville will decode
    from the result set when entities are queried.

  A 'ReadOnlyTableDefinition' can be used with the same select operations as a
  'Orville.PostgreSQL.TableDefinition' via the functions in
  "Orville.PostgreSQL.Execution.SelectableEntityOperations", such as
  'Orville.PostgreSQL.Execution.findSelectableEntitiesBy'.

  See 'mkReadOnlyTableDefinition' for constructing values of this type.

@since 1.2.0.0
-}
data ReadOnlyTableDefinition readEntity = ReadOnlyTableDefinition
  { i_readOnlyTableDefinitionReference :: Expr.TableReference
  , i_readOnlyTableDefinitionMarshaller :: AnnotatedSqlMarshaller () readEntity
  }

{- | The table reference that rows are selected from.

@since 1.2.0.0
-}
readOnlyTableDefinitionReference :: ReadOnlyTableDefinition readEntity -> Expr.TableReference
readOnlyTableDefinitionReference = i_readOnlyTableDefinitionReference

{- | The marshaller that decodes the selected rows. Nothing is ever written
  through a 'ReadOnlyTableDefinition', so the marshaller has no write entity.

@since 1.2.0.0
-}
readOnlyTableDefinitionMarshaller :: ReadOnlyTableDefinition readEntity -> AnnotatedSqlMarshaller () readEntity
readOnlyTableDefinitionMarshaller = i_readOnlyTableDefinitionMarshaller

{- | Applies the provided function to the underlying 'SqlMarshaller' of the
  'ReadOnlyTableDefinition', for instance to reshape the decoded entity with
  @fmap@. The table reference is unchanged.

@since 1.2.0.0
-}
mapReadOnlyTableMarshaller ::
  (SqlMarshaller () readEntityA -> SqlMarshaller () readEntityB) ->
  ReadOnlyTableDefinition readEntityA ->
  ReadOnlyTableDefinition readEntityB
mapReadOnlyTableMarshaller f readOnlyTable =
  readOnlyTable
    { i_readOnlyTableDefinitionMarshaller =
        mapSqlMarshaller f (i_readOnlyTableDefinitionMarshaller readOnlyTable)
    }

{- | Constructs a 'ReadOnlyTableDefinition' from the table reference to select from and the marshaller
  that decodes the selected rows. It is up to the caller to ensure that the columns selected by the
  marshaller make sense for the table reference. Any write fields of the marshaller are ignored.

@since 1.2.0.0
-}
mkReadOnlyTableDefinition ::
  Expr.TableReference ->
  AnnotatedSqlMarshaller writeEntity readEntity ->
  ReadOnlyTableDefinition readEntity
mkReadOnlyTableDefinition tableReference marshaller =
  ReadOnlyTableDefinition
    { i_readOnlyTableDefinitionReference = tableReference
    , i_readOnlyTableDefinitionMarshaller = mapSqlMarshaller marshallReadOnly marshaller
    }
