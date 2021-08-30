{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.ColumnName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.ColumnName
  ( ColumnName,
    columnName,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnName
  = ColumnName RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnName :: String -> ColumnName
columnName = ColumnName . RawSql.fromString
