{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Query.SelectList
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Query.SelectList
  ( SelectList,
    selectColumns,
    deriveColumn,
    deriveColumnAs,
    selectDerivedColumns,
    selectStar,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SelectList = SelectList RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns =
  selectDerivedColumns . map deriveColumn

newtype DerivedColumn = DerivedColumn RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectDerivedColumns :: [DerivedColumn] -> SelectList
selectDerivedColumns derivedColumns =
  SelectList $
    RawSql.intercalate
      RawSql.comma
      (fmap RawSql.toRawSql derivedColumns)

deriveColumn :: ColumnName -> DerivedColumn
deriveColumn =
  DerivedColumn . RawSql.toRawSql

deriveColumnAs :: ColumnName -> ColumnName -> DerivedColumn
deriveColumnAs sourceColumn asColumn =
  DerivedColumn $
    ( RawSql.toRawSql sourceColumn
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asColumn
    )
