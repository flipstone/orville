{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Insert.InsertColumnList
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Insert.InsertColumnList
  ( InsertColumnList,
    insertColumnList,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype InsertColumnList
  = InsertColumnList RawSql.RawSql
  deriving (RawSql.SqlExpression)

insertColumnList :: [ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)
      <> RawSql.rightParen
