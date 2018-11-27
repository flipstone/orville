{-|
Module    : Database.Orville.Core
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.Core
  ( TableDefinition(..)
  , mkTableDefinition
  , tableKeyToSql
  , tableKeyFromSql
  , SqlType(..)
  , serial
  , bigserial
  , text
  , varText
  , integer
  , bigInteger
  , double
  , boolean
  , date
  , timestamp
  , textSearchVector
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
  , fixedTextField
  , dayField
  , utcTimeField
  , int32Field
  , int64Field
  , doubleField
  , boolField
  , automaticIdField
  , searchVectorField
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

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Convertible
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Monoid
import Database.HDBC hiding (withTransaction)

import qualified Data.Map.Helpers as Map
import Database.Orville.Internal.ConstraintDefinition
import Database.Orville.Internal.Execute
import Database.Orville.Internal.Expr
import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.FieldUpdate
import Database.Orville.Internal.FromSql
import Database.Orville.Internal.GroupBy ()
import Database.Orville.Internal.IndexDefinition
import Database.Orville.Internal.MigrateSchema
import Database.Orville.Internal.MigrationError
import Database.Orville.Internal.MigrationPlan
import Database.Orville.Internal.Monad
import Database.Orville.Internal.OrderBy
import Database.Orville.Internal.RelationalMap
import Database.Orville.Internal.SelectOptions
import Database.Orville.Internal.Sql
import Database.Orville.Internal.SqlType
import Database.Orville.Internal.TableDefinition
import Database.Orville.Internal.Types
import Database.Orville.Internal.Where
import Database.Orville.Raw
import Database.Orville.Select

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
  -> m readEntity
insertRecord tableDef newRecord = do
  let builder = tableFromSql tableDef
      returnSelects = expr <$> fromSqlSelects builder
      returnColumns =
        List.intercalate ", " $ map (rawExprToSql . generateSql) returnSelects
      insertSql =
        mkInsertClause
          (tableName tableDef)
          (tableAssignableColumnNames tableDef) ++
        " RETURNING " ++ returnColumns
      vals = runToSql (tableToSql tableDef) newRecord
  rows <-
    withConnection $ \conn -> do
      executingSql InsertQuery insertSql $ do
        insert <- prepare conn insertSql
        void $ execute insert vals
        fetchAllRowsAL' insert
  results <- decodeSqlRows builder rows
  case results of
    [entity] -> pure entity
    [] -> error "Didn't get a record back from the database!"
    _ -> error "Got more than one record back from the database!"

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
