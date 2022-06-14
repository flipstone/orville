module Orville.PostgreSQL.Internal.QueryType
  ( QueryType (SelectQuery, InsertQuery, UpdateQuery, DeleteQuery, DDLQuery, OtherQuery),
  )
where

{- |
  A simple categorization of SQL queries that is used to provide a hint to
  user callbacks about what kind of query is being run.

  See 'Orville.PostgreSQL.addSqlExecutionCallback'
-}
data QueryType
  = SelectQuery
  | InsertQuery
  | UpdateQuery
  | DeleteQuery
  | DDLQuery
  | OtherQuery
  deriving (Ord, Eq, Enum, Bounded, Show, Read)
