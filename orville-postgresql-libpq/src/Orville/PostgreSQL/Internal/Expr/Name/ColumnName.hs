{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.ColumnName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.ColumnName
  ( ColumnName,
    rawColumnName,
    sqlToColumnName,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnName = ColumnName RawSql.RawSql
  deriving (RawSql.SqlExpression)

rawColumnName :: String -> ColumnName
rawColumnName = ColumnName . RawSql.fromString

sqlToColumnName :: RawSql.RawSql -> ColumnName
sqlToColumnName = ColumnName
