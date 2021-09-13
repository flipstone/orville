{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.RowValueExpression
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.RowValueExpression
  ( RowValueExpression,
    columnReference,
    valueExpression,
    rowValueConstructor,
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype RowValueExpression = RowValueExpression RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnReference :: ColumnName -> RowValueExpression
columnReference = RowValueExpression . RawSql.toRawSql

valueExpression :: SqlValue -> RowValueExpression
valueExpression = RowValueExpression . RawSql.parameter

rowValueConstructor :: NE.NonEmpty RowValueExpression -> RowValueExpression
rowValueConstructor elements =
  RowValueExpression $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (map RawSql.toRawSql $ NE.toList elements)
      <> RawSql.rightParen
