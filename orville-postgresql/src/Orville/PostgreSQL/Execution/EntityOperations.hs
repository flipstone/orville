{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Entry-level functions for executing based CRUD operations on entity tables.
These are the functions that you will use most often for interacting with
tables.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.EntityOperations
  ( insertEntity
  , insertEntityAndReturnRowCount
  , insertAndReturnEntity
  , insertEntities
  , insertAndReturnEntities
  , insertEntitiesAndReturnRowCount
  , BatchInsertOption (InOneStatement, InBatches)
  , BatchInsertTransactionality (WithNewTransaction, WithoutNewTransaction)
  , updateEntity
  , updateEntityAndReturnRowCount
  , updateAndReturnEntity
  , updateFields
  , updateFieldsAndReturnEntities
  , updateFieldsAndReturnRowCount
  , ConflictTarget (..)
  , ConflictTargetError (..)
  , upsertEntity
  , upsertEntityAndReturnRowCount
  , upsertAndReturnEntity
  , upsertEntities
  , upsertEntitiesAndReturnRowCount
  , upsertAndReturnEntities
  , deleteEntity
  , deleteEntityAndReturnRowCount
  , deleteAndReturnEntity
  , deleteEntities
  , deleteEntitiesAndReturnRowCount
  , deleteAndReturnEntities
  , findEntitiesBy
  , findFirstEntityBy
  , findEntity
  , findEntities
  )
where

import Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.DList as DList
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (listToMaybe)

import Orville.PostgreSQL.Batchable (BatchSize, Batchable, toBatched, toUnbatched)
import qualified Orville.PostgreSQL.Execution.Delete as Delete
import qualified Orville.PostgreSQL.Execution.Insert as Insert
import qualified Orville.PostgreSQL.Execution.Select as Select
import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Execution.Transaction as Transaction
import qualified Orville.PostgreSQL.Execution.Update as Update
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RowCountExpectation as RowCountExpectation
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Schema as Schema

{- | Inserts an entity into the specified table.

@since 1.0.0.0
-}
insertEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m ()
insertEntity entityTable =
  Monad.void . insertEntityAndReturnRowCount entityTable

{- | Inserts an entity into the specified table. Returns the number of rows
  affected by the query.

@since 1.0.0.0
-}
insertEntityAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m Int
insertEntityAndReturnRowCount entityTable entity =
  insertEntitiesAndReturnRowCount InOneStatement entityTable (entity :| [])

{- | Inserts an entity into the specified table, returning the data inserted into
  the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.

@since 1.0.0.0
-}
insertAndReturnEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m readEntity
insertAndReturnEntity entityTable entity = do
  returnedEntities <- insertAndReturnEntities InOneStatement entityTable (entity :| [])

  RowCountExpectation.expectExactlyOneRow
    "insertAndReturnEntity RETURNING clause"
    returnedEntities

{- | Inserts a non-empty list of entities into the specified table.

@since 1.0.0.0
-}
insertEntities ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m ()
insertEntities batchOption tableDef =
  executeBatchOption_
    batchOption
    Insert.executeInsert
    . Insert.insertToTable tableDef

{- | Inserts a non-empty list of entities into the specified table. Returns the
  number of rows affected by the query.

@since 1.0.0.0
-}
insertEntitiesAndReturnRowCount ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m Int
insertEntitiesAndReturnRowCount batchOption tableDef =
  executeBatchOption
    batchOption
    Insert.executeInsert
    0
    (+)
    . Insert.insertToTable tableDef

{- | Inserts a non-empty list of entities into the specified table, returning the data that
  was inserted into the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.

@since 1.0.0.0
-}
insertAndReturnEntities ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m [readEntity]
insertAndReturnEntities batchOption tableDef =
  fmap DList.toList
    . executeBatchOption
      batchOption
      (fmap DList.fromList . Insert.executeInsertReturnEntities)
      DList.empty
      (<>)
    . Insert.insertToTableReturning tableDef

executeBatchOption ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  (batch -> m batchResult) ->
  fullResult ->
  (fullResult -> batchResult -> fullResult) ->
  Batchable batch ->
  m fullResult
executeBatchOption batchOption runBatch initialAcc combine batchable =
  case batchOption of
    InOneStatement ->
      combine initialAcc <$> runBatch (toUnbatched batchable)
    InBatches batchSize transactionality ->
      let
        -- make sure to force the accumulator to WHNF at each batch
        foldBatch !acc =
          fmap (combine acc) . runBatch
      in
        withBatchInsertTransactionality
          transactionality
          (Monad.foldM foldBatch initialAcc . toBatched batchSize $ batchable)

executeBatchOption_ ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  (batch -> m result) ->
  Batchable batch ->
  m ()
executeBatchOption_ batchOption runBatch batchable =
  case batchOption of
    InOneStatement ->
      Monad.void . runBatch . toUnbatched $ batchable
    InBatches batchSize transactionality ->
      withBatchInsertTransactionality
        transactionality
        (traverse_ runBatch . toBatched batchSize $ batchable)

{- | Controls whether a multi-entity insert is executed as a single SQL
  statement or in multiple batches.

  * 'InOneStatement' sends all entities in a single @INSERT@ statement. This
    is the simplest option but may exceed PostgreSQL's parameter limit for very
    large inserts.

  * @'InBatches' batchSize transactionality@ splits the entities into batches
    and executes a separate @INSERT@ for each batch. The 'BatchSize' controls
    how many entities are included per batch, and the
    'BatchInsertTransactionality' controls whether the batches are wrapped in
    a transaction.

@since 1.2.0.0
-}
data BatchInsertOption
  = InOneStatement
  | InBatches BatchSize BatchInsertTransactionality

{- | Controls whether batched inserts are wrapped in a new transaction.

  * 'WithNewTransaction' wraps all batches in a single transaction, ensuring
    atomicity — either all batches succeed or none do.

  * 'WithoutNewTransaction' does not start a new transaction. Use this when
    you are already inside a transaction or when you do not need atomicity
    across batches.

@since 1.2.0.0
-}
data BatchInsertTransactionality
  = WithNewTransaction
  | WithoutNewTransaction

withBatchInsertTransactionality ::
  Monad.MonadOrville m =>
  BatchInsertTransactionality ->
  m a ->
  m a
withBatchInsertTransactionality transactionality =
  case transactionality of
    WithNewTransaction -> Transaction.withTransaction
    WithoutNewTransaction -> id

{- | Updates the row with the given key with the data given by @writeEntity@.

@since 1.0.0.0
-}
updateEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m ()
updateEntity tableDef key =
  Monad.void . updateEntityAndReturnRowCount tableDef key

{- | Updates the row with the given key with the data given by @writeEntity@.
  Returns the number of rows affected by the query.

@since 1.0.0.0
-}
updateEntityAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m Int
updateEntityAndReturnRowCount tableDef key writeEntity =
  case Update.updateToTable tableDef key writeEntity of
    Nothing ->
      liftIO . throwIO . EmptyUpdateError . Schema.tableIdentifier $ tableDef
    Just update ->
      Update.executeUpdate update

{- | Updates the row with the given key with the data given by @writeEntity@,
  returning the updated row from the database. If no row matches the given key,
  'Nothing' will be returned.

  You can use this function to obtain any column values computed by the database
  during the update, including columns with triggers attached to them.

@since 1.0.0.0
-}
updateAndReturnEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m (Maybe readEntity)
updateAndReturnEntity tableDef key writeEntity =
  case Update.updateToTableReturning tableDef key writeEntity of
    Nothing ->
      liftIO . throwIO . EmptyUpdateError . Schema.tableIdentifier $ tableDef
    Just update -> do
      returnedEntities <- Update.executeUpdateReturnEntities update
      RowCountExpectation.expectAtMostOneRow
        "updateAndReturnEntity RETURNING clause"
        returnedEntities

{- | Applies the given 'Expr.SetClause's to the rows in the table that match the
  given where condition. The easiest way to construct a 'Expr.SetClause' is
  via the 'Orville.Postgresql.setField' function (also exported as @.:=@).

@since 1.0.0.0
-}
updateFields ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  m ()
updateFields tableDef setClauses =
  Monad.void . updateFieldsAndReturnRowCount tableDef setClauses

{- | Applies the given 'Expr.SetClause's to the rows in the table that match the
  given where condition. The easiest way to construct a 'Expr.SetClause' is
  via the 'Orville.Postgresql.setField' function (also exported as @.:=@).
  Returns the number of rows affected by the query.

@since 1.0.0.0
-}
updateFieldsAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  m Int
updateFieldsAndReturnRowCount tableDef setClauses mbWhereCondition =
  Update.executeUpdate $
    Update.updateToTableFields tableDef setClauses mbWhereCondition

{- | Like 'updateFields', but uses a @RETURNING@ clause to return the updated
  version of any rows that were affected by the update.

@since 1.0.0.0
-}
updateFieldsAndReturnEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Maybe Expr.BooleanExpr ->
  m [readEntity]
updateFieldsAndReturnEntities tableDef setClauses mbWhereCondition =
  Update.executeUpdateReturnEntities $
    Update.updateToTableFieldsReturning tableDef setClauses mbWhereCondition

{- | Deletes the row with the given key.

@since 1.0.0.0
-}
deleteEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  m ()
deleteEntity entityTable =
  Monad.void . deleteEntityAndReturnRowCount entityTable

{- | Deletes the row with the given key. Returns the number of rows affected
  by the query.

@since 1.0.0.0
-}
deleteEntityAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  m Int
deleteEntityAndReturnRowCount entityTable key =
  let
    primaryKeyCondition =
      Schema.primaryKeyEquals
        (Schema.tablePrimaryKey entityTable)
        key
  in
    Delete.executeDelete $
      Delete.deleteFromTable entityTable (Just primaryKeyCondition)

{- | Deletes the row with the given key, returning the row that was deleted.
  If no row matches the given key, 'Nothing' is returned.

@since 1.0.0.0
-}
deleteAndReturnEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  m (Maybe readEntity)
deleteAndReturnEntity entityTable key = do
  let
    primaryKeyCondition =
      Schema.primaryKeyEquals
        (Schema.tablePrimaryKey entityTable)
        key

  returnedEntities <- deleteAndReturnEntities entityTable (Just primaryKeyCondition)

  RowCountExpectation.expectAtMostOneRow
    "deleteAndReturnEntity RETURNING clause"
    returnedEntities

{- | Deletes all rows in the given table that match the where condition.

@since 1.0.0.0
-}
deleteEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  m ()
deleteEntities entityTable =
  Monad.void . deleteEntitiesAndReturnRowCount entityTable

{- | Deletes all rows in the given table that match the where condition. Returns
  the number of rows affected by the query.

@since 1.0.0.0
-}
deleteEntitiesAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  m Int
deleteEntitiesAndReturnRowCount entityTable whereCondition =
  Delete.executeDelete $
    Delete.deleteFromTable entityTable whereCondition

{- | Deletes all rows in the given table that match the where condition, returning
  the rows that were deleted.

@since 1.0.0.0
-}
deleteAndReturnEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  m [readEntity]
deleteAndReturnEntities entityTable whereCondition =
  Delete.executeDeleteReturnEntities $
    Delete.deleteFromTableReturning entityTable whereCondition

{- | Finds all the entities in the given table according to the specified
  'SelectOptions.SelectOptions', which may include where conditions to
  match, ordering specifications, etc.

@since 1.0.0.0
-}
findEntitiesBy ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  m [readEntity]
findEntitiesBy entityTable selectOptions =
  Select.executeSelect $
    Select.selectTable entityTable selectOptions

{- | Like 'findEntitiesBy', but adds a 'LIMIT 1' to the query and then returns
  the first item from the list. Usually when you use this you will want to
  provide an order by clause in the 'SelectOptions.SelectOptions' because the
  database will not guarantee ordering.

@since 1.0.0.0
-}
findFirstEntityBy ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  m (Maybe readEntity)
findFirstEntityBy entityTable selectOptions =
  listToMaybe
    <$> findEntitiesBy entityTable (SelectOptions.limit 1 <> selectOptions)

{- | Finds a single entity by the table's primary key value.

@since 1.0.0.0
-}
findEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  m (Maybe readEntity)
findEntity entityTable key =
  let
    primaryKeyCondition =
      Schema.primaryKeyEquals
        (Schema.tablePrimaryKey entityTable)
        key
  in
    findFirstEntityBy entityTable (SelectOptions.where_ primaryKeyCondition)

{- | Finds multiple entities by the table's primary key.

@since 1.0.0.0
-}
findEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty key ->
  m [readEntity]
findEntities entityTable keys =
  let
    primaryKeyCondition =
      Schema.primaryKeyIn
        (Schema.tablePrimaryKey entityTable)
        keys
  in
    findEntitiesBy entityTable (SelectOptions.where_ primaryKeyCondition)

{- | Thrown by 'updateFields' and 'updateFieldsAndReturnEntities' if the
  'Schema.TableDefinition' they are given has no columns to update.

@since 1.0.0.0
-}
newtype EmptyUpdateError
  = EmptyUpdateError Schema.TableIdentifier

-- | @since 1.0.0.0
instance Show EmptyUpdateError where
  show (EmptyUpdateError tableId) =
    "EmptyUdateError: "
      <> Schema.tableIdToString tableId
      <> " has no columns to update."

-- | @since 1.0.0.0
instance Exception EmptyUpdateError

{- |
  Specifies the target for the @ON CONFLICT@ clause of an upsert function.
  See the [PostgreSQL documentation](https://www.postgresql.org/docs/current/sql-insert.html#SQL-ON-CONFLICT) for more information.

@since 1.1.1.0.1
-}
data ConflictTarget where
  {- | Upsert by the primary key of the table. May only be used with tables that have a primary key.

  @since 1.1.1.0.1
  -}
  ByPrimaryKey :: ConflictTarget
  {- | Upsert by a field, assuming PostgreSQL can infer an arbiter constraint/index from the field.

  @since 1.1.1.0.1
  -}
  ByField :: Marshall.FieldDefinition nullability a -> ConflictTarget
  {- | Upsert by the writable fields in a marshaller, assuming PostgreSQL can infer an arbiter constraint/index from the fields.

  @since 1.1.1.0.1
  -}
  ByMarshaller :: Marshall.SqlMarshaller writeEntity readEntity -> ConflictTarget
  {- | Upsert by a unique constraint.

  @since 1.1.1.0.1
  -}
  ByConstraint :: Schema.ConstraintDefinition -> ConflictTarget
  {- | Upsert with a custom 'Expr.ConflictTargetExpr'. This allows usage of more complex arbiter indexes, such
  as a partial unique index.

  @since 1.1.1.0.1
  -}
  ByConflictTargetExpr :: Expr.ConflictTargetExpr -> ConflictTarget

{- |
  An error resulting from attempting to construct an invalid 'Expr.ConflictTargetExpr'.

  Maybe be thrown as an exception by the upsert functions if a valid 'Expr.ConflictTargetExpr' could not
  be constructed for the provided 'ConflictTarget'.

@since 1.1.1.0.1
-}
data ConflictTargetError
  = {- | The provided conflict target was empty.

    @since 1.1.1.0.1
    -}
    EmptyConflictTarget
  | {- | The provided constraint was not a unique constraint.

    @since 1.1.1.0.1
    -}
    NonUniqueConstraintConflictTarget
  | {- | 'ByPrimaryKey' was used with a table that has no primary key.

    @since 1.1.1.0.1
    -}
    NoPrimaryKey

-- | @since 1.1.1.0.1
instance Show ConflictTargetError where
  show EmptyConflictTarget =
    "ConflictTargetError: the provided conflict target was empty."
  show NonUniqueConstraintConflictTarget =
    "ConflictTargetError: the provided constraint was not a unique constraint."
  show NoPrimaryKey =
    "ConflictTargetError: ByPrimaryKey was used with a table that has no primary key."

-- | @since 1.1.1.0.1
instance Exception ConflictTargetError

conflictTargetToConflictTargetExpr ::
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  Either ConflictTargetError Expr.ConflictTargetExpr
conflictTargetToConflictTargetExpr tableDefinition conflictTarget = case conflictTarget of
  ByPrimaryKey ->
    case Schema.tablePrimaryKeyFieldNames tableDefinition of
      Nothing -> Left NoPrimaryKey
      Just fieldNames ->
        Right
          . Expr.conflictTargetForColumnNames
          . fmap Marshall.fieldNameToColumnName
          $ fieldNames
  ByField fieldDefinition ->
    Right
      . Expr.conflictTargetForColumnNames
      . pure
      $ Marshall.fieldColumnName fieldDefinition
  ByMarshaller marshaller ->
    case Marshall.marshallerConflictTargetExpr marshaller of
      Nothing -> Left EmptyConflictTarget
      Just expr -> Right expr
  ByConstraint constraintDef ->
    case Schema.constraintDefinitionConflictTargetExpr constraintDef of
      Nothing -> Left NonUniqueConstraintConflictTarget
      Just expr -> Right expr
  ByConflictTargetExpr expr ->
    Right expr

conflictTargetToConflictTargetExprOrThrow ::
  MonadIO m =>
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  m Expr.ConflictTargetExpr
conflictTargetToConflictTargetExprOrThrow tableDef target =
  case conflictTargetToConflictTargetExpr tableDef target of
    Left err -> liftIO $ throwIO err
    Right expr -> pure expr

{- |
  Similar to 'insertAndReturnEntity', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.

@since 1.1.1.0.1
-}
upsertAndReturnEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  writeEntity ->
  m readEntity
upsertAndReturnEntity tableDef target entity = do
  returnedEntities <- upsertAndReturnEntities InOneStatement tableDef target (entity :| [])

  RowCountExpectation.expectExactlyOneRow
    "upsertAndReturnEntity RETURNING clause"
    returnedEntities

{- |
  Similar to 'insertEntity', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.

@since 1.1.1.0.1
-}
upsertEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  writeEntity ->
  m ()
upsertEntity tableDef target =
  Monad.void . upsertEntityAndReturnRowCount tableDef target

{- |
  Similar to 'insertEntity', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.
  Returns the count of the affected rows.

@since 1.1.1.0.1
-}
upsertEntityAndReturnRowCount ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  writeEntity ->
  m Int
upsertEntityAndReturnRowCount tableDef target =
  upsertEntitiesAndReturnRowCount InOneStatement tableDef target . pure

{- |
  Similar to 'insertAndReturnEntities', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.

@since 1.1.1.0.1
-}
upsertAndReturnEntities ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  NEL.NonEmpty writeEntity ->
  m [readEntity]
upsertAndReturnEntities batchOption tableDef target entities = do
  targetExpr <- conflictTargetToConflictTargetExprOrThrow tableDef target
  fmap DList.toList
    . executeBatchOption
      batchOption
      (fmap DList.fromList . Insert.executeInsertReturnEntities)
      DList.empty
      (<>)
    $ Insert.upsertToTableReturning tableDef targetExpr entities

{- |
  Similar to 'insertEntities', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.

@since 1.1.1.0.1
-}
upsertEntities ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  NEL.NonEmpty writeEntity ->
  m ()
upsertEntities batchOption tableDef target entities = do
  targetExpr <- conflictTargetToConflictTargetExprOrThrow tableDef target
  executeBatchOption_
    batchOption
    Insert.executeInsert
    $ Insert.upsertToTable tableDef targetExpr entities

{- |
  Similar to 'insertEntitiesAndReturnRowCount', but uses an @ON CONFLICT@ clause to update the row if it
  conflicts with an arbiter constraint/index inferred from the provided 'ConflictTarget'.
  Returns the count of the affected rows.

@since 1.1.1.0.1
-}
upsertEntitiesAndReturnRowCount ::
  Monad.MonadOrville m =>
  BatchInsertOption ->
  Schema.TableDefinition key writeEntity readEntity ->
  ConflictTarget ->
  NEL.NonEmpty writeEntity ->
  m Int
upsertEntitiesAndReturnRowCount batchOption tableDef target entities = do
  targetExpr <- conflictTargetToConflictTargetExprOrThrow tableDef target
  executeBatchOption
    batchOption
    Insert.executeInsert
    0
    (+)
    $ Insert.upsertToTable tableDef targetExpr entities
