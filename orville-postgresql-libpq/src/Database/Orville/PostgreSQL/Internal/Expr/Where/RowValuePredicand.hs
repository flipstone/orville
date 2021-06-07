{-|
Module    : Database.Orville.PostgreSQL.Expr.Where.RowValuePredicand
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Where.RowValuePredicand
  ( RowValuePredicand
  , columnReference
  , comparisonValue
  , rowValuePredicandToSql
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName, columnNameToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql    as RawSql
import           Database.Orville.PostgreSQL.Internal.SqlValue  (SqlValue)

newtype RowValuePredicand = RowValuePredicand RawSql.RawSql

rowValuePredicandToSql :: RowValuePredicand -> RawSql.RawSql
rowValuePredicandToSql (RowValuePredicand sql) = sql

columnReference :: ColumnName -> RowValuePredicand
columnReference = RowValuePredicand . columnNameToSql

comparisonValue :: SqlValue -> RowValuePredicand
comparisonValue = RowValuePredicand . RawSql.parameter
