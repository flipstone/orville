{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.ColumnDefinition
  ( ColumnDefinition,
    columnDefinition,
    ColumnConstraint,
    notNullConstraint,
    nullConstraint,
    ColumnDefault,
    columnDefault,
  )
where

import qualified Data.Maybe as Maybe

import Orville.PostgreSQL.Expr.DataType (DataType)
import Orville.PostgreSQL.Expr.Name (ColumnName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ColumnDefinition
  = ColumnDefinition RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnDefinition ::
  ColumnName ->
  DataType ->
  Maybe ColumnConstraint ->
  Maybe ColumnDefault ->
  ColumnDefinition
columnDefinition columnName dataType maybeColumnConstraint maybeColumnDefault =
  ColumnDefinition $
    RawSql.intercalate RawSql.space $
      Maybe.catMaybes
        [ Just $ RawSql.toRawSql columnName
        , Just $ RawSql.toRawSql dataType
        , fmap RawSql.toRawSql maybeColumnConstraint
        , fmap RawSql.toRawSql maybeColumnDefault
        ]

newtype ColumnConstraint
  = ColumnConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

notNullConstraint :: ColumnConstraint
notNullConstraint =
  ColumnConstraint (RawSql.fromString "NOT NULL")

nullConstraint :: ColumnConstraint
nullConstraint =
  ColumnConstraint (RawSql.fromString "NULL")

newtype ColumnDefault
  = ColumnDefault RawSql.RawSql
  deriving (RawSql.SqlExpression)

columnDefault ::
  ValueExpression ->
  ColumnDefault
columnDefault defaultValue =
  ColumnDefault (RawSql.fromString "DEFAULT " <> RawSql.toRawSql defaultValue)
