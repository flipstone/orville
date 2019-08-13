{-|
Module    : Database.Orville.Oracle.Internal.FromClause
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Internal.FromClause where

import Database.Orville.Oracle.Internal.Types

newtype FromClause =
  FromClause String

fromClauseRaw :: String -> FromClause
fromClauseRaw = FromClause

fromClauseTableName :: String -> FromClause
fromClauseTableName name = fromClauseRaw ("FROM " ++ name)

fromClauseTable :: TableDefinition readEntity writeEntity key -> FromClause
fromClauseTable = fromClauseTableName . tableName

fromClauseToSql :: FromClause -> String
fromClauseToSql (FromClause sql) = sql
