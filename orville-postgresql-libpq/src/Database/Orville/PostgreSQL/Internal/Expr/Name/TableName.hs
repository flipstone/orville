{-|
Module    : Database.Orville.PostgreSQL.Expr.Name.TableName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Name.TableName
  ( TableName
  , tableNameToSql
  , rawTableName
  ) where

import Database.Orville.PostgreSQL.Internal.RawSql
  ( RawSql
  , fromString
  )

newtype TableName =
  TableName RawSql

tableNameToSql :: TableName -> RawSql
tableNameToSql (TableName sql) = sql

rawTableName :: String -> TableName
rawTableName =
  TableName . fromString
