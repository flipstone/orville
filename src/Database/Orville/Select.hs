{-|
Module    : Database.Orville.Select
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE RankNTypes #-}

module Database.Orville.Select
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

import Database.Orville.Internal.Execute
import Database.Orville.Internal.FromClause
import Database.Orville.Internal.FromSql
import Database.Orville.Internal.Monad
import Database.Orville.Internal.Select

runSelect :: Select row -> Orville [row]
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
