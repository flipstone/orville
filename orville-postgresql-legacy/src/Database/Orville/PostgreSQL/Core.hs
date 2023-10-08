{-|
Module    : Database.Orville.PostgreSQL.Core
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT

Migration Guide: Although not all exports are identical, most of the items in
this module can now be imported from @Orville.PostgreSQL@.

Please note that the new LibPQ-based version of orville represents a complete
re-write of Orville from the ground up. As such many of the APIs have been
re-thought with the goal of providing stability and better experience long
term.

Major changes:

* The library no longer allows the connection type to vary. It is intended only
  to be used with PostgreSQL. Thus 'MonadOrville' and other types that used to
  have a @conn@ type parameter no longer have that parameter.

* 'OrvilleT' has been removed in favor of simply using @ReaderT@. For trivial
  cases (i.e. @ReaderT@ over @IO@) a pre-packaged @Orville@ monad is provided.

* In 'TableDefinition', the order of the type parameters has changed from
  @TableDefinition readEnity writeEntity key@ to @TableDefinition key
  writeEntity readEntity@. This make it more consistent with the order of these
  arguments in other types. 'TableParams' has been removed in favor of building
  a basic table definition with the require parameters first and adding or
  setting other optional items after initial construction.

* 'RelationalMap' has been replaced by @SqlMarshaller@. Many functions have
  been renamed, but most functions have a direct or nearly direct translation
  from the old ones. See the docs on the individual functions such as
  'attrField' to see what has changed.

* The auto-migration system is significantly improved. Standard indexes no
  longer need to be given explicit names. Indexes and constraints are now
  attached to 'TableDefinition' rather than being their own schema items.
  Indexes and constraints are no longer dropped explicitly -- they are dropped
  automatically by Orville when they have been removed from the table
  definiton.

* A number of places that previously accepted @[]@ now require @NonEmpty@
  instead. This is done in places where an empty list would not be valid SQL.
  For examples of this change see 'insertRecordMany' and 'updateFields'.

* 'whereAnd' and 'whereOr' (which took lists) have been replaced
  with binary boolean logic functions @andExpr@ and @orExpr@. These functions
  also have operator aliases (@(.&&)@, @(.||)@).

The following items exported from this module have migration guide notes
available in their documentation:

* 'TableDefinition'
* 'mkTableDefinition'
* 'TableParams'
* 'RelationalMap'
* 'fields'
* 'mapAttr'
* 'mapField'
* 'attrField'
* 'maybeMapper'
* 'prefixMap'
* 'partialMap'
* 'readOnlyMap'
* 'readOnlyField'
* 'OrvilleEnv'
* 'newOrvilleEnv'
* 'setStartTransactionSQL'
* 'aroundRunningQuery'
* 'addTransactionCallBack'
* 'OrvilleT'
* 'HasOrvilleContext'
* 'MonadOrville'
* 'runOrville'
* 'mapOrvilleT'
* 'MonadOrvilleControl'
* 'defaultLiftWithConnection'
* 'defaultLiftFinally'
* 'withCachedConnection'
* 'withTransaction'
* 'ColumnFlag'
* 'FieldDefinition'
* 'isFieldNullable'
* 'fieldOfType'
* 'textField'
* 'fixedTextField'
* 'unboundedTextField'
* 'dayField'
* 'utcTimeField'
* 'int32Field'
* 'int64Field'
* 'doubleField'
* 'boolField'
* 'automaticIdField'
* 'searchVectorField'
* 'nullableField'
* 'foreignKeyField'
* 'withFlag'
* 'withName'
* 'withConversion'
* 'fieldFromSql'
* 'fieldToSqlValue'
* 'SomeField'
* 'withPrefix'
* 'fieldFlags'
* 'uniqueIndex'
* 'simpleIndex'
* 'uniqueConstraint'
* 'dropConstraint'
* 'SchemaItem'
* 'SchemaDefinition'
* 'Record'
* 'WhereCondition'
* 'whereAnd'
* 'whereOr'
* 'whereIn'
* 'whereLike'
* 'whereLikeInsensitive'
* 'whereNotIn'
* 'whereQualified'
* 'whereRaw'
* 'whereToSql'
* 'isNull'
* 'isNotNull'
* 'migrateSchema'
* 'generateMigrationPlan'
* 'MigrationPlan'
* 'MigrationItem'
* 'migrationPlanItems'
* 'selectAll'
* 'selectFirst'
* 'deleteRecord'
* 'deleteWhere'
* 'findRecord'
* 'findRecords'
* 'findRecordsBy'
* 'insertRecord'
* 'insertRecordMany'
* 'insertRecordManyReturning'
* 'updateFields'
* 'updateRecord'
* 'createIndexesConcurrently'
* 'dropIndexesConcurrently'

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
  , OrvilleEnv -- migration guide added
  , newOrvilleEnv -- migration guide added
  , setStartTransactionSQL -- migration guide added
  , aroundRunningQuery -- migration guide added
  , addTransactionCallBack -- migration guide added
  , ormEnvPool
  , TransactionEvent(..)
  , OrvilleT -- migration guide added
  , unOrvilleT
  , SqlValue
  , HasOrvilleContext(..) -- migration guide added
  , MonadOrville -- migration guide added
  , runOrville -- migration guide added
  , mapOrvilleT -- migration guide added
  , MonadOrvilleControl(..) -- migration guide added
  , defaultLiftWithConnection -- migration guide added
  , defaultLiftFinally -- migration guide added
  , QueryType(..)
  , withCachedConnection -- migration guide added
  , withTransaction -- migration guide added
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
  , uniqueIndex -- migration guide added
  , simpleIndex -- migration guide added
  , simplePartialIndex
  , uniquePartialIndex
  , ConstraintDefinition(..)
  , SequenceDefinition(..)
  , uniqueConstraint -- migration guide added
  , dropConstraint -- migration guide added
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
  , SchemaItem(..) -- migration guide added
  , SchemaDefinition -- migration guide added
  , Record -- migration guide added
  , CreatedAt
  , UpdatedAt
  , OccurredAt
  , TableComments
  , noComments
  , say
  , WhereCondition -- migration guide added
  , whereAnd -- migration guide added
  , whereOr -- migration guide added
  , whereIn -- migration guide added
  , whereLike -- migration guide added
  , whereLikeInsensitive -- migration guide added
  , whereNotIn -- migration guide added
  , whereQualified -- migration guide added
  , whereRaw -- migration guide added
  , whereToSql -- migration guide added
  , isNull -- migration guide added
  , isNotNull -- migration guide added
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
  , migrateSchema -- migration guide added
  , MigrationError(..)
  , generateMigrationPlan -- migration guide added
  , MigrationPlan -- migration guide added
  , MigrationItem(..) -- migration guide added
  , migrationPlanItems -- migration guide added
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
  , createIndexesConcurrently -- migration guide added
  , dropIndexesConcurrently -- migration guide added
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
