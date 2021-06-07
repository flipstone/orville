{-|
Module    : Database.Orville.PostgreSQL.Expr.Name.ColumnName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Name.ColumnName
  ( ColumnName
  , rawColumnName
  , columnNameToSql
  ) where

import           Database.Orville.PostgreSQL.Internal.RawSql (RawSql, fromString)

newtype ColumnName = ColumnName RawSql

columnNameToSql :: ColumnName -> RawSql
columnNameToSql (ColumnName sql) = sql

rawColumnName :: String -> ColumnName
rawColumnName = ColumnName . fromString
