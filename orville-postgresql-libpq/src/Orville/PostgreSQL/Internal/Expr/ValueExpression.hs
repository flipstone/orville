{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.ValueExpression
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.ValueExpression
  ( ValueExpression,
    columnReference,
    valueExpression,
    rowValueConstructor,
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype ValueExpression = ValueExpression RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnReference :: ColumnName -> ValueExpression
columnReference = ValueExpression . RawSql.toRawSql

valueExpression :: SqlValue -> ValueExpression
valueExpression = ValueExpression . RawSql.parameter

rowValueConstructor :: NE.NonEmpty ValueExpression -> ValueExpression
rowValueConstructor elements =
  ValueExpression $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma elements
      <> RawSql.rightParen
