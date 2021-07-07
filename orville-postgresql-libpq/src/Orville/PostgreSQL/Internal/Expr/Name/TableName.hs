{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.TableName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.TableName
  ( TableName,
    rawTableName,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableName
  = TableName RawSql.RawSql
  deriving (RawSql.ToRawSql)

rawTableName :: String -> TableName
rawTableName =
  TableName . RawSql.fromString
