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

import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnName = ColumnName RawSql.RawSql

columnNameToSql :: ColumnName -> RawSql.RawSql
columnNameToSql (ColumnName sql) = sql

rawColumnName :: String -> ColumnName
rawColumnName = ColumnName . fromString
