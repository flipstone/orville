{-|
Module    : Database.Orville.PostgreSQL.Internal.FromClause
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.FromClause where

import Database.Orville.PostgreSQL.Internal.Sql
import Database.Orville.PostgreSQL.Internal.Types

newtype FromClause =
  FromClause String

fromClauseRaw :: String -> FromClause
fromClauseRaw = FromClause

fromClauseTableName :: String -> FromClause
fromClauseTableName name = fromClauseRaw ("FROM " ++ escapedName name)

fromClauseTable :: TableDefinition readEntity writeEntity key -> FromClause
fromClauseTable = fromClauseTableName . tableName

fromClauseToSql :: FromClause -> String
fromClauseToSql (FromClause sql) = sql
