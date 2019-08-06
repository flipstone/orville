{-|
Module    : Database.Orville.PostgreSQL.Internal.QueryCache
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Orville.PostgreSQL.Internal.QueryCache
  ( QueryCached
  , runQueryCached
  , selectCached
  , selectFirstCached
  , findRecordCached
  , findRecordsCached
  , findRecordsByCached
  , unsafeLift
  ) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans
import Control.Monad.Trans.State
import qualified Data.Map as Map
import qualified Data.Map.Helpers as Map
import Data.Maybe

import Data.String (fromString)

import Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))

import Database.Orville.PostgreSQL.Internal.Expr
import Database.Orville.PostgreSQL.Internal.FromSql
import Database.Orville.PostgreSQL.Internal.Monad
import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.SelectOptions
import Database.Orville.PostgreSQL.Internal.TableDefinition
import Database.Orville.PostgreSQL.Internal.Types
import Database.Orville.PostgreSQL.Internal.Where
import Database.Orville.PostgreSQL.Select

type QueryCache = Map.Map QueryKey ResultSet

newtype QueryCached m a =
  QueryCached (StateT QueryCache m a)
  deriving (Functor, Applicative, Monad)

runQueryCached :: Monad m => QueryCached m a -> m a
runQueryCached (QueryCached statet) = evalStateT statet Map.empty

cached ::
     Monad m => QueryKey -> QueryCached m ResultSet -> QueryCached m ResultSet
cached key action = do
  cache <- QueryCached get
  case Map.lookup key cache of
    Just result -> do
      pure result
    Nothing -> do
      result <- action
      QueryCached $ put (Map.insert key result cache)
      pure result

selectCachedRows ::
     (MonadThrow m, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> QueryCached m ResultSet
selectCachedRows tableDef opts =
  cached key $
  unsafeLift $
  runSelect $ selectQueryRows selects (fromClauseTable tableDef) opts
  where
    selects = expr . selectColumn . fromString <$> tableColumnNames tableDef
    key = mconcat [queryKey tableDef, queryKey opts]

selectCached ::
     (MonadThrow m, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> QueryCached m [readEntity]
selectCached tableDef opts = do
  rows <- selectCachedRows tableDef opts
  unsafeLift $ decodeSqlRows (tableFromSql tableDef) rows

selectFirstCached ::
     (MonadThrow m, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> QueryCached m (Maybe readEntity)
selectFirstCached tableDef opts =
  listToMaybe <$> selectCached tableDef (limit 1 <> opts)

findRecordsCached ::
     (MonadThrow m, MonadOrville conn m, Ord key)
  => TableDefinition readEntity writeEntity key
  -> [key]
  -> QueryCached m (Map.Map key readEntity)
findRecordsCached tableDef keys = do
  let keyField = tablePrimaryKey tableDef
      mkEntry record = (tableGetKey tableDef record, record)
  recordList <- selectCached tableDef (where_ $ keyField .<- keys)
  pure $ Map.fromList (map mkEntry recordList)

findRecordCached ::
     (MonadThrow m, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> key
  -> QueryCached m (Maybe readEntity)
findRecordCached tableDef key =
  let keyField = tablePrimaryKey tableDef
   in selectFirstCached tableDef (where_ $ keyField .== key)

findRecordsByCached ::
     (Ord fieldValue, MonadThrow m, MonadOrville conn m)
  => TableDefinition readEntity writeEntity key
  -> FieldDefinition fieldValue
  -> SelectOptions
  -> QueryCached m (Map.Map fieldValue [readEntity])
findRecordsByCached tableDef field opts = do
  let builder = (,) <$> fieldFromSql field <*> tableFromSql tableDef
  rows <- selectCachedRows tableDef opts
  Map.groupBy' id <$> unsafeLift (decodeSqlRows builder rows)

-- this is unsafe in the sense that it does not provide
-- any guarantees that the action won't chance values in
-- the database, rendering the cache incorrect. It is not
-- exposed publically, but all usages of it here need to
-- be examined for correctness manually.
--
unsafeLift :: Monad m => m a -> QueryCached m a
unsafeLift = QueryCached . lift
