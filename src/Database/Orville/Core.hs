{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.Core
  ( SqlValue
  , Orville
  , OrvilleT, unOrvilleT, OrvilleEnv, newOrvilleEnv, ormEnvPool
  , MonadOrville(..), runOrville, mapOrvilleT
  , withTransaction

  , ColumnFlag (..)
  , Now(..)
  , ColumnType (..)
  , FieldDefinition, withPrefix
  , TableDefinition (..)
  , tablePrimaryKey
  , mkTableDefinition
  , TableParams(..)
  , RelationalMap
  , mapAttr, mapField, attrField
  , maybeMapper, prefixMap, partialMap, readOnlyMap

  , IndexDefinition (..)
  , uniqueIndex, simpleIndex

  , ConstraintDefinition (..)
  , uniqueConstraint, dropConstraint

  , FromSql
  , FromSqlError(..)
  , ColumnSpecifier(..)
  , col
  , nullableCol
  , nextColumn

  , ToSql
  , getField
  , getComponent
  , withFlag
  , withName

  , SchemaItem(..)
  , SchemaDefinition
  , Record
  , CreatedAt

  , TableComments
  , noComments, say

  , WhereCondition
  , whereAnd, whereOr, whereIn, whereNotIn, isNull, isNotNull
  , (.==), (.<>), (.<-), (%==), (.>), (.>=), (.<), (.<=)

  , SelectOptions(..)
  , where_, order, limit, offset
  , (<>)

  , FieldUpdate(..)
  , fieldUpdate, (.:=)

  , OrderByClause (..)
  , SortDirection (..)
  , migrateSchema
  , selectAll
  , selectFirst
  , deleteRecord
  , deleteWhere
  , findRecord
  , findRecords
  , findRecordsBy
  , insertRecord
  , insertRecordMany
  , updateFields
  , updateRecord
  ) where

import            Control.Monad.Except
import            Control.Monad.Reader
import            Control.Monad.State
import            Data.Convertible
import qualified  Data.List as List
import            Data.Maybe (listToMaybe)
import qualified  Data.Map.Strict as Map
import            Data.Monoid
import            Database.HDBC hiding (withTransaction)

import qualified  Data.Map.Helpers as Map
import            Database.Orville.Internal.ConstraintDefinition
import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.FieldUpdate
import            Database.Orville.Internal.FromSql
import            Database.Orville.Internal.IndexDefinition
import            Database.Orville.Internal.Monad
import            Database.Orville.Internal.MigrateSchema
import            Database.Orville.Internal.OrderBy
import            Database.Orville.Internal.RelationalMap
import            Database.Orville.Internal.SelectOptions
import            Database.Orville.Internal.TableDefinition
import            Database.Orville.Internal.Types
import            Database.Orville.Internal.Where
import            Database.Orville.Raw

getField :: Convertible a SqlValue
         => (entity -> a) -> ToSql entity ()
getField f = do
  value <- asks f
  sqlValues <- get
  put (convert value : sqlValues)

selectWhereBuild :: TableDefinition entity
                 -> FromSql result
                 -> SelectOptions
                 -> Orville [result]
selectWhereBuild tableDef builder opts =
    selectSql querySql (selectOptValues opts) builder
  where
    selectClause = mkSelectClause tableDef
    querySql = List.intercalate " " [
                     selectClause
                   , selectOptClause opts
                   ]

selectAll :: TableDefinition entity
          -> SelectOptions
          -> Orville [entity Record]
selectAll tableDef = selectWhereBuild tableDef (tableFromSql tableDef)

selectFirst :: TableDefinition entity
            -> SelectOptions
            -> Orville (Maybe (entity Record))
selectFirst tableDef opts = listToMaybe <$> selectAll tableDef (limit 1 <> opts)

deleteWhereBuild :: TableDefinition entity
                 -> [WhereCondition]
                 -> Orville Integer
deleteWhereBuild tableDef conds = do
  let deleteSql = mkDeleteClause tableDef
  let whereSql = whereClause conds
  let values = whereValues conds
  let querySql = deleteSql ++ " " ++ whereSql

  withConnection $ \conn -> liftIO $ do
    catchSqlErr querySql (do
                          deletedCount <- run conn querySql values
                          return deletedCount)

deleteWhere :: TableDefinition entity
            -> [WhereCondition]
            -> Orville Integer
deleteWhere tableDef = deleteWhereBuild tableDef

findRecords :: TableDefinition entity
            -> [Record]
            -> Orville (Map.Map Record (entity Record))
findRecords _ [] = return Map.empty
findRecords tableDef keys = do
  let keyField = tablePrimaryKey tableDef
      mkEntry record = (tableGetKey tableDef record, record)

  recordList <- selectAll tableDef (where_ $ keyField .<- keys)
  pure $ Map.fromList (map mkEntry recordList)

findRecordsBy :: (Convertible SqlValue fieldValue, Ord fieldValue)
              => TableDefinition entity
              -> FieldDefinition
              -> SelectOptions
              -> Orville (Map.Map fieldValue [entity Record])
findRecordsBy tableDef field opts = do
  let builder = (,) <$> col field <*> tableFromSql tableDef
  Map.groupBy' id <$> selectWhereBuild tableDef builder opts

findRecord :: TableDefinition entity
           -> Record
           -> Orville (Maybe (entity Record))
findRecord tableDef key = do
  let keyField = tablePrimaryKey tableDef
  selectFirst tableDef (where_ $ keyField .== key)

updateFields :: TableDefinition entity
             -> [FieldUpdate]
             -> [WhereCondition]
             -> Orville Integer
updateFields tableDef updates conds =
    updateSql (updateClause ++ " " ++ condClause)
              (updateValues ++ condValues)
  where
    condClause = whereClause conds
    condValues = whereValues conds

    updateValues = map fieldUpdateValue updates
    updateClause = mkUpdateClause tableDef updates

updateRecord :: TableDefinition entity
             -> Record
             -> entity key
             -> Orville (entity Record)
updateRecord tableDef recordId record = do
  let keyField = tablePrimaryKey tableDef
      conds = [keyField .== recordId]

      fields = filter (not . isUninsertedField)
                      (tableFields tableDef)

      builder = tableToSql tableDef

      updates = zipWith FieldUpdate
                        fields
                        (runToSql builder record)

  void $ updateFields tableDef updates conds

  pure $ tableSetKey tableDef recordId record


insertRecord :: TableDefinition entity
             -> entity ()
             -> Orville (entity Record)
insertRecord tableDef newRecord = do
  let insertSql = mkInsertClause tableDef ++ " RETURNING id"
  let builder = tableToSql tableDef

  rows <- withConnection $ \conn -> liftIO $ do
    putStrLn insertSql
    let vals = (runToSql builder newRecord)
    print vals

    insert <- prepare conn insertSql
    catchSqlErr insertSql (do
                           void $ execute insert vals
                           rows <- fetchAllRows' insert
                           return rows)

  case rows of
    [[key]] -> case safeConvert key of
               Right int -> return $ tableSetKey tableDef int newRecord
               _ -> error "Got a non-integer key back from the db!"

    [] -> error "Didn't get a key back from the database!"
    _ -> error "Got more than one key back from the database!"

insertRecordMany :: TableDefinition entity
                 -> [entity ()]
                 -> Orville ()
insertRecordMany tableDef newRecords = do
  let insertSql = mkInsertClause tableDef
  let builder = tableToSql tableDef

  withConnection $ \conn -> liftIO $ do
    catchSqlErr insertSql (do
                           insert <- prepare conn insertSql
                           executeMany insert (map (runToSql builder) newRecords))

deleteRecord :: TableDefinition entity
             -> entity Record
             -> Orville ()
deleteRecord tableDef record = do
  let keyField = tablePrimaryKey tableDef

  n <- deleteWhere tableDef
                   [keyField .== tableGetKey tableDef record]

  if n /= 1
    then error $ "Expected to delete exactly 1 row for deleteRecord\
                 \but actually deleted" ++ show n
    else pure ()

