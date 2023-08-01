{- |
Copyright : Flipstone Technology Partners 2021-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Execution.EntityOperations
  ( insertEntity
  , insertAndReturnEntity
  , insertEntities
  , insertAndReturnEntities
  , updateEntity
  , updateAndReturnEntity
  , updateFields
  , updateFieldsAndReturnEntities
  , deleteEntity
  , deleteAndReturnEntity
  , deleteEntities
  , deleteAndReturnEntities
  , findEntitiesBy
  , findFirstEntityBy
  , findEntity
  , findEntities
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Execution.Delete as Delete
import qualified Orville.PostgreSQL.Execution.Insert as Insert
import qualified Orville.PostgreSQL.Execution.Select as Select
import qualified Orville.PostgreSQL.Execution.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Execution.Update as Update
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.RowCountExpectation as RowCountExpectation
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Schema as Schema

{- |
  Inserts a entity into the specified table. Returns the number of rows
  affected by the query.

@since 0.10.0.0
-}
insertEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m Int
insertEntity entityTable entity =
  insertEntities entityTable (entity :| [])

{- |
  Inserts a entity into the specified table, returning the data inserted into
  the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.

@since 0.10.0.0
-}
insertAndReturnEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m readEntity
insertAndReturnEntity entityTable entity = do
  returnedEntities <- insertAndReturnEntities entityTable (entity :| [])

  RowCountExpectation.expectExactlyOneRow
    "insertAndReturnEntity RETURNING clause"
    returnedEntities

{- |
  Inserts a non-empty list of entities into the specified table. Returns the
  number of rows affected by the query.

@since 0.10.0.0
-}
insertEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m Int
insertEntities tableDef =
  Insert.executeInsert . Insert.insertToTable tableDef

{- |
  Inserts a non-empty list of entities into the specified table, returning the data that
  was inserted into the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.

@since 0.10.0.0
-}
insertAndReturnEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m [readEntity]
insertAndReturnEntities tableDef =
  Insert.executeInsertReturnEntities . Insert.insertToTableReturning tableDef

{- |
  Updates the row with the given key in with the data given by 'writeEntity'.
  Returns the number of rows affected by the query.

@since 0.10.0.0
-}
updateEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m Int
updateEntity tableDef key writeEntity =
  case Update.updateToTable tableDef key writeEntity of
    Nothing ->
      liftIO . throwIO . EmptyUpdateError . Schema.tableIdentifier $ tableDef
    Just update ->
      Update.executeUpdate update

{- |
  Updates the row with the given key in with the data given by 'writeEntity',
  returning updated row from the database. If no row matches the given key,
  'Nothing' will be returned.

  You can use this function to obtain any column values computer by the database
  during update, including columns with triggers attached to them.

@since 0.10.0.0
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

{- |
  Applies the given 'Expr.SetClause's to the rows in the table that match the
  given where condition. The easiest way to construct a 'Expr.SetClause' is
  via the 'Orville.Postgresql.setField' function (also exported as @.:=@).
  Returns the number of rows affected by the query.

@since 0.10.0.0
-}
updateFields ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Expr.BooleanExpr ->
  m Int
updateFields tableDef setClauses whereCondition =
  Update.executeUpdate $
    Update.updateToTableFields tableDef setClauses (Just whereCondition)

{- |
  Like 'updateFields', but uses a @RETURNING@ clause to return the updated
  version of any rows that were affected by the update.

@since 0.10.0.0
-}
updateFieldsAndReturnEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  NonEmpty Expr.SetClause ->
  Expr.BooleanExpr ->
  m [readEntity]
updateFieldsAndReturnEntities tableDef setClauses whereCondition =
  Update.executeUpdateReturnEntities $
    Update.updateToTableFieldsReturning tableDef setClauses (Just whereCondition)

{- |
  Deletes the row with the given key. Returns the number of rows affected
  by the query.

@since 0.10.0.0
-}
deleteEntity ::
  Monad.MonadOrville m =>
  Schema.TableDefinition (Schema.HasKey key) writeEntity readEntity ->
  key ->
  m Int
deleteEntity entityTable key =
  let
    primaryKeyCondition =
      Schema.primaryKeyEquals
        (Schema.tablePrimaryKey entityTable)
        key
  in
    Delete.executeDelete $
      Delete.deleteFromTable entityTable (Just primaryKeyCondition)

{- |
  Deletes the row with the given key, returning the row that was deleted.
  If no row matches the given key, 'Nothing' is returned.

@since 0.10.0.0
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

{- |
  Deletes all rows in the given table that match the where condition. Returns
  the number of rows affected by the query.

@since 0.10.0.0
-}
deleteEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  m Int
deleteEntities entityTable whereCondition =
  Delete.executeDelete $
    Delete.deleteFromTable entityTable whereCondition

{- |
  Deletes all rows in the given table that match the where condition, returning
  the rows that were deleted.

@since 0.10.0.0
-}
deleteAndReturnEntities ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  Maybe Expr.BooleanExpr ->
  m [readEntity]
deleteAndReturnEntities entityTable whereCondition =
  Delete.executeDeleteReturnEntities $
    Delete.deleteFromTableReturning entityTable whereCondition

{- |
  Finds all the entities in the given table according to the specified
  'SelectOptions.SelectOptions', which may include where conditions to
  match, ordering specifications, etc.

@since 0.10.0.0
-}
findEntitiesBy ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  m [readEntity]
findEntitiesBy entityTable selectOptions =
  Select.executeSelect $
    Select.selectTable entityTable selectOptions

{- |
  Like 'findEntitiesBy, but adds a 'LIMIT 1' to the query and then returns
  the first item from the list. Usually when you use this you will want to
  provide an order by clause in the 'SelectOptions.SelectOptions' because the
  database will not guarantee ordering.

@since 0.10.0.0
-}
findFirstEntityBy ::
  Monad.MonadOrville m =>
  Schema.TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  m (Maybe readEntity)
findFirstEntityBy entityTable selectOptions =
  listToMaybe
    <$> findEntitiesBy entityTable (SelectOptions.limit 1 <> selectOptions)

{- |
  Finds a single entity by the table's primary key value.

@since 0.10.0.0
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

{- |
  Finds multiple entities by the table's primary key.

@since 0.10.0.0
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

{- |
  Thrown by 'updateFields' and 'updateFieldsAndReturnEntities' if the
  'Schema.TableDefinition' they are given has no columns to update.

@since 0.10.0.0
-}
newtype EmptyUpdateError
  = EmptyUpdateError Schema.TableIdentifier

-- | @since 0.10.0.0
instance Show EmptyUpdateError where
  show (EmptyUpdateError tableId) =
    "EmptyUdateError: "
      <> Schema.tableIdToString tableId
      <> " has no columns to update."

-- | @since 0.10.0.0
instance Exception EmptyUpdateError
