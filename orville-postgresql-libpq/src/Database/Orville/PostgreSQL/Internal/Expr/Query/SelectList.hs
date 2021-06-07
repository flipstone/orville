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

import           Database.Orville.PostgreSQL.Internal.Expr.Name (ColumnName,
                                                                 columnNameToSql)
import           Database.Orville.PostgreSQL.Internal.RawSql    (RawSql,
                                                                 fromString,
                                                                 intercalate)

newtype SelectList = SelectList RawSql

selectStar :: SelectList
selectStar =
  SelectList (fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns columnNames =
  SelectList $
    intercalate
      (fromString ",")
      (fmap columnNameToSql columnNames)

selectListToSql :: SelectList -> RawSql
selectListToSql (SelectList sql) =
  sql
