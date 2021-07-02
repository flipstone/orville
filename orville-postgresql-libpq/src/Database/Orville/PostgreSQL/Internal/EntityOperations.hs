module Database.Orville.PostgreSQL.Internal.EntityOperations
  ( insertEntity,
    insertEntities,
    updateEntity,
    findEntitiesBy,
    findFirstEntityBy,
    findEntity,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)

import qualified Database.Orville.PostgreSQL.Internal.MonadOrville as MonadOrville
import qualified Database.Orville.PostgreSQL.Internal.PrimaryKey as PrimaryKey
import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SelectOptions as SelectOptions
import qualified Database.Orville.PostgreSQL.Internal.SqlMarshaller as SqlMarshaller
import qualified Database.Orville.PostgreSQL.Internal.TableDefinition as TableDef

{- |
  Inserts a entity into the specified table.

  TODO: This should return the 'readEntity' type using using the psql RETURNING
  clause and decoding the result set.
-}
insertEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  writeEntity ->
  m ()
insertEntity entityTable entity =
  insertEntities entityTable (entity :| [])

{- |
  Inserts a set of entities into the specified table
-}
insertEntities ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  NonEmpty writeEntity ->
  m ()
insertEntities entityTable entities =
  let insertExpr =
        TableDef.mkInsertExpr entityTable entities
   in MonadOrville.withConnection $ \connection ->
        liftIO $
          RawSql.executeVoid connection insertExpr

{- |
  Updates the row with the given key in with the data given by 'writeEntity'
-}
updateEntity ::
  MonadOrville.MonadOrville m =>
  TableDef.TableDefinition key writeEntity readEntity ->
  key ->
  writeEntity ->
  m ()
updateEntity tableDef key writeEntity =
  MonadOrville.withConnection $ \connection ->
    liftIO $
      RawSql.executeVoid
        connection
        (TableDef.mkUpdateExpr tableDef key writeEntity)

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
  let selectExpr =
        TableDef.mkQueryExpr
          entityTable
          (SelectOptions.selectWhereClause selectOptions)
          Nothing
          Nothing
   in do
        libPqResult <-
          MonadOrville.withConnection $ \connection ->
            liftIO $
              RawSql.execute connection selectExpr

        liftIO $ do
          decodingResult <-
            SqlMarshaller.marshallResultFromSql
              (TableDef.tableMarshaller entityTable)
              libPqResult

          case decodingResult of
            Left err ->
              throwIO err
            Right entities ->
              pure entities

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
  TableDef.TableDefinition key writeEntity readEntity ->
  key ->
  m (Maybe readEntity)
findEntity entityTable key =
  let primaryKeyCondition =
        PrimaryKey.primaryKeyEquals
          (TableDef.tablePrimaryKey entityTable)
          key
   in findFirstEntityBy entityTable (SelectOptions.where_ primaryKeyCondition)
