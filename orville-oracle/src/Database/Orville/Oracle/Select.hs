{-|
Module    : Database.Orville.Oracle.Select
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Select
  ( Select
  , selectQuery
  , selectQueryTable
  , selectQueryRows
  , selectQueryRaw
  , selectQueryRawRows
  , selectQueryColumns
  , selectField
  , FromClause
  , fromClauseRaw
  , fromClauseTableName
  , fromClauseTable
  , runSelect
  ) where

import Control.Monad (void)
import Database.HDBC

import Database.Orville.Oracle.Internal.Execute
import Database.Orville.Oracle.Internal.FromClause
import Database.Orville.Oracle.Internal.FromSql
import Database.Orville.Oracle.Internal.Monad
import Database.Orville.Oracle.Internal.Select

runSelect :: MonadOrville conn m => Select row -> m [row]
runSelect select = do
  rows <-
    withConnection $ \conn -> do
      executingSql SelectQuery sql $ do
        query <- prepare conn sql
        void $ execute query values
        fetchAllRowsAL' query
  decodeSqlRows builder rows
  where
    sql = selectSql select
    values = selectValues select
    builder = selectBuilder select
