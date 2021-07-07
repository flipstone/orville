{- |
Module    : Orville.PostgreSQL.Expr.Query.SelectList
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.Query.SelectList
  ( SelectList,
    selectColumns,
    selectStar,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SelectList = SelectList RawSql.RawSql
  deriving RawSql.SqlExpression

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns columnNames =
  SelectList $
    RawSql.intercalate
      RawSql.comma
      (fmap RawSql.toRawSql columnNames)
