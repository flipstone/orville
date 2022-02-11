{- |
Module    : Orville.PostgreSQL.Internal.EntityOperations
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.EntityOperations
  ( insertEntity,
    insertAndReturnEntity,
    insertEntities,
    insertAndReturnEntities,
    updateEntity,
    updateAndReturnEntity,
    deleteEntity,
    deleteAndReturnEntity,
    findEntitiesBy,
    findFirstEntityBy,
    findEntity,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)

import qualified Orville.PostgreSQL.Internal.Execute as Execute
import qualified Orville.PostgreSQL.Internal.Insert as Insert
import qualified Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Orville.PostgreSQL.Internal.ReturningOption as ReturningOption
import qualified Orville.PostgreSQL.Internal.Select as Select
import qualified Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Orville.PostgreSQL.Internal.TableDefinition as TableDef
import qualified Orville.PostgreSQL.Internal.Update as Update

{- |
  Inserts a entity into the specified table.
-}
insertEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m ()
insertEntity entityTable entity =
  insertEntities entityTable (entity :| [])

{- |
  Inserts a entity into the specified table, returning the data inserted into
  the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.
-}
insertAndReturnEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m readEntity
insertAndReturnEntity entityTable entity = do
  returnedEntities <- insertAndReturnEntities entityTable (entity :| [])

  case returnedEntities of
    [returnedEntity] ->
      pure returnedEntity
    _ ->
      liftIO . throwIO . RowCountExpectationError $
        "Expected exactly one row to be returned in RETURNING clause, but got " <> show (length returnedEntities)

{- |
  Inserts a non-empty list of entities into the specified table
-}
insertEntities ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m ()
insertEntities tableDef =
  Insert.executeInsert . Insert.insertToTable tableDef

{- |
  Inserts a non-empty list of entities into the specified table, returning the data that
  was inserted into the database.

  You can use this function to obtain any column values filled in by the
  database, such as auto-incrementing ids.
-}
insertAndReturnEntities ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m [readEntity]
insertAndReturnEntities tableDef =
  Insert.executeInsertReturnEntities . Insert.insertToTableReturning tableDef

{- |
  Updates the row with the given key in with the data given by 'writeEntity'
-}
updateEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition (TableDef.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m ()
updateEntity tableDef key =
  Update.executeUpdate . Update.updateToTable tableDef key

{- |
  Updates the row with the given key in with the data given by 'writeEntity',
  returning updated row from the database. If no row matches the given key,
  'Nothing' will be returned.

  You can use this function to obtain any column values computer by the database
  during update, including columns with triggers attached to them.
-}
updateAndReturnEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition (TableDef.HasKey key) writeEntity readEntity ->
  key ->
  writeEntity ->
  m (Maybe readEntity)
updateAndReturnEntity tableDef key =
  Update.executeUpdateReturnEntity . Update.updateToTableReturning tableDef key

{- |
  Deletes the row with the given key
-}
deleteEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition (TableDef.HasKey key) writeEntity readEntity ->
  key ->
  m ()
deleteEntity tableDef key =
  Execute.executeVoid $
    TableDef.mkDeleteExpr ReturningOption.WithoutReturning tableDef key

{- |
  Deletes the row with the given key, returning the row that was deleted.
  If no row matches the given key, 'Nothing' is returned.
-}
deleteAndReturnEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition (TableDef.HasKey key) writeEntity readEntity ->
  key ->
  m (Maybe readEntity)
deleteAndReturnEntity tableDef key =
  fmap listToMaybe $
    Execute.executeAndDecode
      (TableDef.mkDeleteExpr ReturningOption.WithReturning tableDef key)
      (TableDef.tableMarshaller tableDef)

{- |
  Finds all the entities in the given table according to the specified
  'SelectOptions.SelectOptions', which may include where conditions to
  match, ordering specifications, etc.
-}
findEntitiesBy ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
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
-}
findFirstEntityBy ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  SelectOptions.SelectOptions ->
  m (Maybe readEntity)
findFirstEntityBy entityTable selectOptions =
  listToMaybe
    <$> findEntitiesBy entityTable (SelectOptions.limit 1 <> selectOptions)

{- |
  Finds a single entity by the table's primary key value.
-}
findEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition (TableDef.HasKey key) writeEntity readEntity ->
  key ->
  m (Maybe readEntity)
findEntity entityTable key =
  let primaryKeyCondition =
        PrimaryKey.primaryKeyEquals
          (TableDef.tablePrimaryKey entityTable)
          key
   in findFirstEntityBy entityTable (SelectOptions.where_ primaryKeyCondition)

{- |
  INTERNAL: This should really never get thrown in the real world. It would be
  thrown if the returning clause from an insert statement for a single record
  returned 0 records or more than 1 record.
-}
newtype RowCountExpectationError
  = RowCountExpectationError String
  deriving (Show)

instance Exception RowCountExpectationError
