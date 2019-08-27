{-|
Module    : Database.Orville.Oracle.Core
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.Oracle.Core
  ( TableDefinition(..)
  , mkTableDefinition
  , tableKeyToSql
  , tableKeyFromSql
  , SqlType(..)
  , text
  , integer
  , bigInteger
  , double
  , boolean
  , date
  , timestamp
  , convertSqlType
  , maybeConvertSqlType
  , nullableType
  , TableParams(..)
  , RelationalMap
  , mapAttr
  , mapField
  , attrField
  , maybeMapper
  , prefixMap
  , partialMap
  , readOnlyMap
  , readOnlyField
  , OrvilleEnv
  , newOrvilleEnv
  , setStartTransactionSQL
  , aroundRunningQuery
  , addTransactionCallBack
  , ormEnvPool
  , TransactionEvent(..)
  , OrvilleT
  , unOrvilleT
  , SqlValue
  , HasOrvilleContext(..)
  , MonadOrville
  , runOrville
  , mapOrvilleT
  , MonadOrvilleControl(..)
  , defaultLiftWithConnection
  , defaultLiftFinally
  , QueryType(..)
  , withTransaction
  , ColumnFlag(..)
  , Now(..)
  , ColumnType(..)
  , FieldDefinition
  , fieldOfType
  , textField
  , dayField
  , localTimeField
  , utcTimeField
  , int32Field
  , int64Field
  , doubleField
  , boolField
  , integerNumberField
  , doubleNumberField
  , nullableField
  , foreignKeyField
  , withFlag
  , withName
  , withConversion
  , fieldFromSql
  , fieldToSqlValue
  , SomeField(..)
  , withPrefix
  , fieldName
  , fieldType
  , fieldFlags
  , IndexDefinition(..)
  , uniqueIndex
  , simpleIndex
  , ConstraintDefinition(..)
  , uniqueConstraint
  , dropConstraint
  , FromSql
  , FromSqlError(..)
  , ColumnSpecifier(..)
  , col
  , ToSql
  , getField
  , getComponent
  , SchemaItem(..)
  , SchemaDefinition
  , Record
  , CreatedAt
  , UpdatedAt
  , OccurredAt
  , TableComments
  , noComments
  , say
  , WhereCondition
  , whereAnd
  , whereOr
  , whereIn
  , whereLike
  , whereLikeInsensitive
  , whereNotIn
  , whereQualified
  , whereRaw
  , isNull
  , isNotNull
  , (.==)
  , (.<>)
  , (.<-)
  , (%==)
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , SelectOptions(..)
  , where_
  , distinct
  , order
  , limit
  , offset
  , groupBy
  , (<>)
  , FieldUpdate
  , fieldUpdate
  , (.:=)
  , OrderByClause(..)
  , SortDirection(..)
  , migrateSchema
  , MigrationError(..)
  , generateMigrationPlan
  , MigrationPlan
  , MigrationItem(..)
  , migrationPlanItems
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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Convertible
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Database.HDBC hiding (withTransaction)

import Database.Orville.Oracle.Internal.MappendCompat ((<>))

import qualified Data.Map.Helpers as Map
import Database.Orville.Oracle.Internal.ConstraintDefinition
import Database.Orville.Oracle.Internal.Execute
import Database.Orville.Oracle.Internal.FieldDefinition
import Database.Orville.Oracle.Internal.FieldUpdate
import Database.Orville.Oracle.Internal.FromSql
import Database.Orville.Oracle.Internal.GroupBy ()
import Database.Orville.Oracle.Internal.IndexDefinition
import Database.Orville.Oracle.Internal.MigrateSchema
import Database.Orville.Oracle.Internal.MigrationError
import Database.Orville.Oracle.Internal.MigrationPlan
import Database.Orville.Oracle.Internal.Monad
import Database.Orville.Oracle.Internal.OrderBy
import Database.Orville.Oracle.Internal.RelationalMap
import Database.Orville.Oracle.Internal.SelectOptions
import Database.Orville.Oracle.Internal.Sql
import Database.Orville.Oracle.Internal.SqlType
import Database.Orville.Oracle.Internal.TableDefinition
import Database.Orville.Oracle.Internal.Types
import Database.Orville.Oracle.Internal.Where
import Database.Orville.Oracle.Raw
import Database.Orville.Oracle.Select

getField :: Convertible a SqlValue => (entity -> a) -> ToSql entity ()
getField f = do
  value <- asks f
  sqlValues <- get
  put (convert value : sqlValues)

selectAll ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> m [readEntity]
selectAll tableDef = runSelect . selectQueryTable tableDef

selectFirst ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> m (Maybe readEntity)
selectFirst tableDef opts =
  listToMaybe <$> selectAll tableDef (limit 1 <> opts)

deleteWhereBuild ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [WhereCondition]
  -> m Integer
deleteWhereBuild tableDef conds = do
  let deleteSql = mkDeleteClause (tableName tableDef)
  let whereSql = whereClause conds
  let values = whereValues conds
  let querySql = deleteSql ++ " " ++ whereSql
  withConnection $ \conn -> do
    executingSql DeleteQuery querySql $ do run conn querySql values

deleteWhere ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [WhereCondition]
  -> m Integer
deleteWhere tableDef = deleteWhereBuild tableDef

findRecords ::
     (Ord key, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> [key]
  -> m (Map.Map key readEntity)
findRecords _ [] = return Map.empty
findRecords tableDef keys = do
  let keyField = tablePrimaryKey tableDef
      mkEntry record = (tableGetKey tableDef record, record)
  recordList <- selectAll tableDef (where_ $ keyField .<- keys)
  pure $ Map.fromList (map mkEntry recordList)

findRecordsBy ::
     (Ord fieldValue, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> FieldDefinition fieldValue
  -> SelectOptions
  -> m (Map.Map fieldValue [readEntity])
findRecordsBy tableDef field opts = do
  let builder = (,) <$> fieldFromSql field <*> tableFromSql tableDef
      query = selectQuery builder (fromClauseTable tableDef) opts
  Map.groupBy' id <$> runSelect query

findRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> m (Maybe readEntity)
findRecord tableDef key =
  let keyField = tablePrimaryKey tableDef
   in selectFirst tableDef (where_ $ keyField .== key)

updateFields ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [FieldUpdate]
  -> [WhereCondition]
  -> m Integer
updateFields tableDef updates conds =
  updateSql (updateClause ++ " " ++ condClause) (updateValues ++ condValues)
  where
    condClause = whereClause conds
    condValues = whereValues conds
    updateValues = map fieldUpdateValue updates
    updateNames = map fieldUpdateName updates
    updateClause = mkUpdateClause (tableName tableDef) updateNames

updateRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> writeEntity
  -> m ()
updateRecord tableDef key record = do
  let keyField = tablePrimaryKey tableDef
      conds = [keyField .== key]
      fields = tableAssignableFields tableDef
      builder = tableToSql tableDef
      updates = zipWith FieldUpdate fields (runToSql builder record)
  void $ updateFields tableDef updates conds

insertRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> writeEntity
  -> m Integer
insertRecord tableDef newRecord = do
  let insertSql =
        mkInsertClause
          (tableName tableDef)
          (tableAssignableColumnNames tableDef)
      vals = runToSql (tableToSql tableDef) newRecord
  numRows <-
    withConnection $ \conn -> do
      executingSql InsertQuery insertSql $ do
        insert <- prepare conn insertSql
        execute insert vals
  case numRows of
    0 -> error "Insert did not modify any rows!"
    x | x < 0 -> error "A driver error occured during insert!" -- This should never happen according HDBC docs..
    x -> pure x

insertRecordMany ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [writeEntity]
  -> m ()
insertRecordMany tableDef newRecords = do
  let insertSql =
        mkInsertClause
          (tableName tableDef)
          (tableAssignableColumnNames tableDef)
  let builder = tableToSql tableDef
  withConnection $ \conn -> do
    executingSql InsertQuery insertSql $ do
      insert <- prepare conn insertSql
      executeMany insert (map (runToSql builder) newRecords)

deleteRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> m ()
deleteRecord tableDef key = do
  let keyField = tablePrimaryKey tableDef
  n <- deleteWhere tableDef [keyField .== key]
  if n /= 1
    then error $
         "Expected to delete exactly 1 row for deleteRecord\
               \but actually deleted" ++
         show n
    else pure ()
