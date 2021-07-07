{- |
Module    : Orville.PostgreSQL.Expr.Where.RowValuePredicand
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.RowValuePredicand
  ( RowValuePredicand,
    columnReference,
    comparisonValue,
    rowValuePredicandToSql,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype RowValuePredicand = RowValuePredicand RawSql.RawSql

rowValuePredicandToSql :: RowValuePredicand -> RawSql.RawSql
rowValuePredicandToSql (RowValuePredicand sql) = sql

columnReference :: ColumnName -> RowValuePredicand
columnReference = RowValuePredicand . RawSql.toRawSql

comparisonValue :: SqlValue -> RowValuePredicand
comparisonValue = RowValuePredicand . RawSql.parameter
