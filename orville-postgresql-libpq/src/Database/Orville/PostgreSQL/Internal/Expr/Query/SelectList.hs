{-|
Module    : Database.Orville.PostgreSQL.Expr.Query.SelectList
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Query.SelectList
  ( SelectList
  , selectListToSql
  , selectColumns
  , selectStar
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName, columnNameToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql    as RawSql

newtype SelectList = SelectList RawSql.RawSql

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns columnNames =
  SelectList $
    RawSql.intercalate
      (RawSql.fromString ",")
      (fmap columnNameToSql columnNames)

selectListToSql :: SelectList -> RawSql.RawSql
selectListToSql (SelectList sql) =
  sql
