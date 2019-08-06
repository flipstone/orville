{-|
Module    : Database.Orville.PostgreSQL.Internal.GroupBy
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Orville.PostgreSQL.Internal.GroupBy where

import Database.HDBC

import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.Types

data GroupByClause =
  GroupByClause String
                [SqlValue]

instance QueryKeyable GroupByClause where
  queryKey (GroupByClause sql vals) = QKList [QKField sql, queryKey vals]

groupingSql :: GroupByClause -> String
groupingSql (GroupByClause sql _) = sql ++ " "

groupingValues :: GroupByClause -> [SqlValue]
groupingValues (GroupByClause _ values) = values

class ToGroupBy a where
  toGroupBy :: a -> GroupByClause

instance ToGroupBy (FieldDefinition a) where
  toGroupBy fieldDef = GroupByClause (fieldName fieldDef) []

instance ToGroupBy (String, [SqlValue]) where
  toGroupBy (sql, values) = GroupByClause sql values
