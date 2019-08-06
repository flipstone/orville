{-|
Module    : Database.Orville.PostgreSQL.Internal.OrderBy
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Orville.PostgreSQL.Internal.OrderBy where

import Database.HDBC

import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.Types

data SortDirection
  = Ascending
  | Descending
  deriving (Show)

instance QueryKeyable SortDirection where
  queryKey dir = QKOp (sqlDirection dir) QKEmpty

sqlDirection :: SortDirection -> String
sqlDirection Ascending = "ASC"
sqlDirection Descending = "DESC"

data OrderByClause =
  OrderByClause String
                [SqlValue]
                SortDirection

instance QueryKeyable OrderByClause where
  queryKey (OrderByClause sql vals dir) =
    QKList [QKField sql, queryKey vals, queryKey dir]

sortingSql :: OrderByClause -> String
sortingSql (OrderByClause sql _ sortDir) = sql ++ " " ++ sqlDirection sortDir

sortingValues :: OrderByClause -> [SqlValue]
sortingValues (OrderByClause _ values _) = values

class ToOrderBy a where
  toOrderBy :: a -> SortDirection -> OrderByClause

instance ToOrderBy (FieldDefinition a) where
  toOrderBy fieldDef = OrderByClause (fieldName fieldDef) []

instance ToOrderBy (String, [SqlValue]) where
  toOrderBy (sql, values) = OrderByClause sql values
