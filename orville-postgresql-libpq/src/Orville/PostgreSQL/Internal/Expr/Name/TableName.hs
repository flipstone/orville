{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.TableName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.TableName
  ( TableName,
    tableName,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableName
  = TableName RawSql.RawSql
  deriving (RawSql.SqlExpression)

tableName :: String -> TableName
tableName =
  TableName . RawSql.fromString
