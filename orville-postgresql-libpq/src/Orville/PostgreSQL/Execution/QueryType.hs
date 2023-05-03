{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Execution.QueryType
  ( QueryType (SelectQuery, InsertQuery, UpdateQuery, DeleteQuery, DDLQuery, CursorQuery, OtherQuery),
  )
where

{- |
  A simple categorization of SQL queries that is used to provide a hint to
  user callbacks about what kind of query is being run.

  See 'Orville.PostgreSQL.addSqlExecutionCallback'

@since 0.10.0.0
-}
data QueryType
  = SelectQuery
  | InsertQuery
  | UpdateQuery
  | DeleteQuery
  | DDLQuery
  | CursorQuery
  | OtherQuery
  deriving
    ( -- | @since 0.10.0.0
      Ord
    , -- | @since 0.10.0.0
      Eq
    , -- | @since 0.10.0.0
      Enum
    , -- | @since 0.10.0.0
      Bounded
    , -- | @since 0.10.0.0
      Show
    , -- | @since 0.10.0.0
      Read
    )
