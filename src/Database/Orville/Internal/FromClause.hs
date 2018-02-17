{-|
Module    : Database.Orville.Internal.FromClause
Copyright : Fliptsone Technology Partners 2016-2018
License   : MIT
-}

module Database.Orville.Internal.FromClause where

import            Database.Orville.Internal.Sql
import            Database.Orville.Internal.Types

newtype FromClause = FromClause String

fromClauseRaw :: String -> FromClause
fromClauseRaw = FromClause

fromClauseTableName :: String -> FromClause
fromClauseTableName name = fromClauseRaw ("FROM " ++ escapedName name)

fromClauseTable :: TableDefinition entity -> FromClause
fromClauseTable = fromClauseTableName . tableName

fromClauseToSql :: FromClause -> String
fromClauseToSql (FromClause sql) = sql
