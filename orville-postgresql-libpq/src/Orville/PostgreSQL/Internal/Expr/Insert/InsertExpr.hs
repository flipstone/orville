{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Internal.Expr.Insert.InsertExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Insert.InsertExpr
  ( InsertExpr,
    insertExpr,
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.Expr.Insert.InsertColumnList (InsertColumnList)
import Orville.PostgreSQL.Internal.Expr.Insert.InsertSource (InsertSource)
import Orville.PostgreSQL.Internal.Expr.Name (Qualified, TableName)
import Orville.PostgreSQL.Internal.Expr.ReturningExpr (ReturningExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertExpr ::
  Qualified TableName ->
  Maybe InsertColumnList ->
  InsertSource ->
  Maybe ReturningExpr ->
  InsertExpr
insertExpr target maybeInsertColumns source maybeReturning =
  InsertExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "INSERT INTO"
        , Just $ RawSql.toRawSql target
        , fmap RawSql.toRawSql maybeInsertColumns
        , Just $ RawSql.toRawSql source
        , fmap RawSql.toRawSql maybeReturning
        ]
