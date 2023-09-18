{-|
Module    : Database.Orville.PostgreSQL.Core
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT

Migration Guide: Although not all exports are identical, most of the items in
this module can now be imported from @Orville.PostgreSQL@.

-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.PostgreSQL.Core
  ( TableDefinition(..) -- migration guide added
  , PrimaryKey
  , primaryKeyIn
  , primaryKeyEquals
  , primaryKeyDescription
  , primaryKeyToSql
  , primaryKey
  , compositePrimaryKey
  , primaryKeyPart
  , mkTableDefinition -- migration guide added
  , SqlType(..)
  , serial
  , bigserial
  , text
  , varText
  , unboundedText
  , integer
  , bigInteger
  , double
  , boolean
  , date
  , timestamp
  , textSearchVector
  , convertSqlType -- migration guide added
  , maybeConvertSqlType -- migration guide added
  , TableParams(..) -- migration guide added
  , RelationalMap -- migration guide added
  , fields -- migration guide added
  , mapAttr -- migration guide added
  , mapField -- migration guide added
  , attrField -- migration guide added
  , maybeMapper -- migration guide added
  , prefixMap -- migration guide added
  , partialMap -- migration guide added
  , readOnlyMap -- migration guide added
  , readOnlyField -- migration guide added
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
  , withCachedConnection
  , withTransaction
  , ColumnFlag(..) -- migration guide added
  , ColumnDefault(toColumnDefaultSql)
  , Now(..)
  , FieldDefinition -- migration guide added
  , Nullable
  , NotNull
  , Nullability(..)
  , isFieldNullable -- migration guide added
  , fieldOfType -- migration guide added
  , textField -- migration guide added
  , fixedTextField -- migration guide added
  , unboundedTextField -- migration guide added
  , dayField -- migration guide added
  , utcTimeField -- migration guide added
  , int32Field -- migration guide added
  , int64Field -- migration guide added
  , doubleField -- migration guide added
  , boolField -- migration guide added
  , automaticIdField -- migration guide added
  , searchVectorField -- migration guide added
  , nullableField -- migration guide added
  , foreignKeyField -- migration guided added
  , withFlag -- migration guide added
  , withName -- migration guide added
  , withConversion -- migration guide added
  , fieldFromSql -- migration guide added
  , fieldToSqlValue -- migration guide added
  , SomeField(..) -- migration guide added
  , withPrefix -- migration guide added
  , fieldName
  , fieldType
  , fieldFlags -- migration guide added
  , IndexDefinition(..)
  , uniqueIndex
  , simpleIndex
  , simplePartialIndex
  , uniquePartialIndex
  , ConstraintDefinition(..)
  , SequenceDefinition(..)
  , uniqueConstraint
  , dropConstraint
  , FromSql
  , FromSqlError(..)
  , RowDataErrorDetails(..)
  , RowDataErrorReason(..)
  , MissingColumnDetails(..)
  , ConversionErrorDetails(..)
  , showFromSqlErrorMinimal
  , showFromSqlErrorForLogging
  , showSqlValueType
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
  , whereToSql
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
  , selectOptionsToSql
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
  , Pagination(..)
  , buildPagination
  , selectAll -- migration guide added
  , selectFirst -- migraiton guide added
  , deleteRecord -- migration guide added
  , deleteWhere -- migration guide added
  , findRecord -- migration guide added
  , findRecords -- migration guide added
  , findRecordsBy -- migration guide added
  , insertRecord -- migration guide added
  , insertRecordMany -- migration guide added
  , insertRecordManyReturning -- migration guide added
  , updateFields -- migration guide added
  , updateRecord -- migration guide added
  , sequenceNextVal
  , sequenceSetVal
  , sequenceCurrVal
  , createIndexesConcurrently
  , dropIndexesConcurrently
  ) where

import Control.Monad (void, when)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Convertible
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Database.HDBC hiding (withTransaction)

import Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))

import qualified Data.Map.Helpers as Map
import Database.Orville.PostgreSQL.Internal.ConstraintDefinition
import Database.Orville.PostgreSQL.Internal.Execute
import Database.Orville.PostgreSQL.Internal.Expr
import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.FieldUpdate
import Database.Orville.PostgreSQL.Internal.FromSql
import Database.Orville.PostgreSQL.Internal.GroupBy ()
import Database.Orville.PostgreSQL.Internal.IndexDefinition
import Database.Orville.PostgreSQL.Internal.MigrateSchema
import Database.Orville.PostgreSQL.Internal.MigrationError
import Database.Orville.PostgreSQL.Internal.MigrationPlan
import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.OrderBy
import Database.Orville.PostgreSQL.Internal.PrimaryKey
import Database.Orville.PostgreSQL.Internal.RelationalMap
import Database.Orville.PostgreSQL.Internal.SelectOptions
import Database.Orville.PostgreSQL.Internal.Sql
import Database.Orville.PostgreSQL.Internal.SqlType
import Database.Orville.PostgreSQL.Internal.TableDefinition
import Database.Orville.PostgreSQL.Internal.Types
import Database.Orville.PostgreSQL.Internal.Where
import Database.Orville.PostgreSQL.Pagination
import Database.Orville.PostgreSQL.Raw
import Database.Orville.PostgreSQL.Select

getField :: Convertible a SqlValue => (entity -> a) -> ToSql entity ()
getField f = do
  value <- asks f
  sqlValues <- get
  put (convert value : sqlValues)

{- |
  Migration Guide: @selectAll@ has been renamed to @findEntitiesBy@
-}
selectAll ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> m [readEntity]
selectAll tableDef = runSelect . selectQueryTable tableDef


{- |
  Migration Guide: @selectFirst@ has been renamed to @findFirstEntityBy@
-}
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

{- |
  Migration Guide: @deleteWhere@ has been renamed to @deleteEntities@. It
  now takes a @Maybe BooleanExpr@ rather than @[WhereCondition]@
-}
deleteWhere ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [WhereCondition]
  -> m Integer
deleteWhere tableDef = deleteWhereBuild tableDef

{- |
  Migration Guide: @findRecords@ has been renamed to @findEntities@. It now
  requires a @NonEmpty key@ rather than simply @[key]@ and returns a
  @[readEntity]@ instead of a @Map@.
-}
findRecords ::
     (Ord key, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> [key]
  -> m (Map.Map key readEntity)
findRecords _ [] = return Map.empty
findRecords tableDef keys = do
  let keyDef = tablePrimaryKey tableDef
      mkEntry record = (tableGetKey tableDef record, record)
  recordList <- selectAll tableDef (where_ $ primaryKeyIn keyDef keys)
  pure $ Map.fromList (map mkEntry recordList)

{- |
  Migration Guide: @findRecordsBy@ has been renamed to @findEntitiesBy@. It
  no longer takes a @FieldDefinition@ to group by. Instead it simply returns
  a @[readEntity]@
-}
findRecordsBy ::
     (Ord fieldValue, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> FieldDefinition nullability fieldValue
  -> SelectOptions
  -> m (Map.Map fieldValue [readEntity])
findRecordsBy tableDef field opts = do
  let builder = (,) <$> fieldFromSql field <*> tableFromSql tableDef
      query = selectQuery builder (fromClauseTable tableDef) opts
  Map.groupBy' id <$> runSelect query

{- |
  Migration Guide: @findRecord@ has been renamed to @findEntity@
-}
findRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> m (Maybe readEntity)
findRecord tableDef key =
  let keyDef = tablePrimaryKey tableDef
   in selectFirst tableDef (where_ $ primaryKeyEquals keyDef key)

{- |
  Migration Guide: @updateFields@ has been renamed to
  @updateFieldsAndReturnRowCount@, but now takes a @NonEmpty SetClause@ instead
  of a @[Field Update]@ and a @Maybe BooleanExpr@ instead of a
  @[WhereCondition]@.

  @updateFields@ still exists as a variant of this function, but returns @()@
  rather than @Int@. @updateFieldsAndReturnEntities@ is now available as well.
-}
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

{- |
  Migration Guide: @updateRecord@ has been renamed to @updateEntity. Note that
  there are also new variant functions @updateAndReturnEntity@ and
  @updateEntityAndReturnRowCount@.
-}
updateRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> writeEntity
  -> m ()
updateRecord tableDef key record = do
  let keyDef = tablePrimaryKey tableDef
      conds = [primaryKeyEquals keyDef key]
      fields = tableAssignableFields tableDef
      builder = tableToSql tableDef
      updates = zipWith FieldUpdate fields (runToSql builder record)
  void $ updateFields tableDef updates conds

{- |
  Migration Guide: @insertRecord@ has been renamed to @insertAndReturnEntity@.
  Note there are also new variant functions @insertEntity@ and
  @insertEntityAndReturnRowCount@ that return @()@ and @Int@ respectively.
-}
insertRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> writeEntity
  -> m readEntity
insertRecord tableDef newRecord = do
  results <- insertRecordManyReturning tableDef [newRecord]
  case results of
    [entity] -> pure entity
    [] -> error "Didn't get a record back from the database!"
    _ -> error "Got more than one record back from the database!"

{- |
  Migration Guide: @insertRecordManyReturning@ has been renamed to
  @insertAndReturnEntities@.
-}
insertRecordManyReturning ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [writeEntity]
  -> m [readEntity]
insertRecordManyReturning _ [] = pure []
insertRecordManyReturning tableDef newRecords = do
  let builder = tableFromSql tableDef
      returnSelects = expr <$> fromSqlSelects builder
      returnColumns =
        List.intercalate ", " $ map (rawExprToSql . generateSql) returnSelects
      insertSql =
        mkInsertManyClause
          (tableName tableDef)
          (tableAssignableColumnNames tableDef)
          (length newRecords) ++
        " RETURNING " ++ returnColumns
      vals = concatMap (runToSql $ tableToSql tableDef) newRecords
  rows <-
    withConnection $ \conn -> do
      executingSql InsertQuery insertSql $ do
        insert <- prepare conn insertSql
        void $ execute insert vals
        fetchAllRowsAL' insert
  decodeSqlRows builder rows

{- |
  Migration Guide: @insertRecordMany@ has been renamed to @insertEntities@. It
  now requires a @NonEmpty writeEntity@ rather than @[writeEntity]@. Note that
  there are also new variant functions @insertAndReturnEntities@ and
  @insertEntitiesAndReturnRowCount@.
-}
insertRecordMany ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> [writeEntity]
  -> m ()
insertRecordMany tableDef newRecords = do
  let insertSql =
        mkInsertManyClause
          (tableName tableDef)
          (tableAssignableColumnNames tableDef)
          (length newRecords)
  let builder = tableToSql tableDef
  when (not $ null newRecords) $
    withConnection $ \conn -> do
      executingSql InsertQuery insertSql $ do
        insert <- prepare conn insertSql
        void $ execute insert (concatMap (runToSql builder) newRecords)

{- |
  Migration Guide: @deleteRecord@ has been renamed to @deleteEntity@. Note
  that there are also new variant functions @deleteAndReturnEntity@ and
  @deleteEntityAndReturnRowCount@ that return @Maybe readEntity@ and @Int@
  respectively.
-}
deleteRecord ::
     MonadOrville conn m
  => TableDefinition readEntity writeEntity key
  -> key
  -> m ()
deleteRecord tableDef key = do
  let keyDef = tablePrimaryKey tableDef
  n <- deleteWhere tableDef [primaryKeyEquals keyDef key]
  if n /= 1
    then error $
         "Expected to delete exactly 1 row for deleteRecord\
               \but actually deleted" ++
         show n
    else pure ()

sequenceNextVal ::
     MonadOrville conn m
  => SequenceDefinition
  -> m Int
sequenceNextVal seqDef = do
  n <- selectSql "SELECT nextval(?)"
                 [SqlString $ sequenceName seqDef]
                 (fieldFromSql $ int64Field "nextval")
  case n of
    [r] -> pure $ fromIntegral r
    _ -> error $ "Failed to execute nextval for sequence " ++ sequenceName seqDef ++ "!"

sequenceSetVal ::
     MonadOrville conn m
  => SequenceDefinition
  -> Int
  -> m Int
sequenceSetVal seqDef v = do
  n <- selectSql "SELECT setval(?, ?)"
                 [SqlString $ sequenceName seqDef, SqlInt64 $ fromIntegral v]
                 (fieldFromSql $ int64Field "setval")
  case n of
    [r] -> pure $ fromIntegral r
    _ -> error $ "Failed to execute setval for sequence " ++ sequenceName seqDef ++ "!"

sequenceCurrVal ::
     MonadOrville conn m
  => SequenceDefinition
  -> m Int
sequenceCurrVal seqDef = do
  n <- selectSql "SELECT currval(?)"
                 [SqlString $ sequenceName seqDef]
                 (fieldFromSql $ int64Field "currval")
  case n of
    [r] -> pure $ fromIntegral r
    _ -> error $ "Failed to get current value for sequence " ++ sequenceName seqDef ++ "!"