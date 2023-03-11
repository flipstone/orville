{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.ValueExpression
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.ValueExpression
  ( ValueExpression,
    cast,
    ParameterName,
    columnReference,
    valueExpression,
    rowValueConstructor,
    functionCall,
    functionCallNamedParams,
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.Name (ColumnName, FunctionName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import Orville.PostgreSQL.Internal.SqlValue (SqlValue)

newtype ValueExpression = ValueExpression RawSql.RawSql
  deriving (RawSql.SqlExpression)

cast :: ValueExpression -> DataType -> ValueExpression
cast value dataType =
  ValueExpression $
    RawSql.toRawSql value
      <> RawSql.fromString "::"
      <> RawSql.toRawSql dataType

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

functionCall :: FunctionName -> [ValueExpression] -> ValueExpression
functionCall functionName parameters =
  ValueExpression $
    RawSql.toRawSql functionName
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma parameters
      <> RawSql.rightParen

newtype ParameterName = ParameterName RawSql.RawSql
  deriving (RawSql.SqlExpression)

functionCallNamedParams :: FunctionName -> [(ParameterName, ValueExpression)] -> ValueExpression
functionCallNamedParams functionName parameters =
  ValueExpression $
    RawSql.toRawSql functionName
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap (uncurry namedParameterArgument) parameters)
      <> RawSql.rightParen

namedParameterArgument :: ParameterName -> ValueExpression -> RawSql.RawSql
namedParameterArgument name value =
  RawSql.toRawSql name
    <> RawSql.space
    <> RawSql.fromString "=>"
    <> RawSql.space
    <> RawSql.toRawSql value
