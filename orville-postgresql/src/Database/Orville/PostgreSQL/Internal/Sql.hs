{-|
Module    : Database.Orville.PostgreSQL.Internal.Sql
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Sql where

import qualified Data.List as List

import Database.Orville.PostgreSQL.Internal.Expr

mkInsertClause :: String -> [String] -> String
mkInsertClause tblName columnNames =
  "INSERT INTO " ++
  escapedName tblName ++ " (" ++ columns ++ ") VALUES (" ++ placeholders ++ ")"
  where
    escapedColumnNames = rawExprToSql . generateSql . NameForm Nothing <$> columnNames
    columns = List.intercalate "," escapedColumnNames
    placeholders = List.intercalate "," $ map (const "?") columnNames

mkUpdateClause :: String -> [String] -> String
mkUpdateClause tblName columnNames =
  "UPDATE " ++ escapedName tblName ++ " SET " ++ placeholders
  where
    escapedColumnNames = rawExprToSql . generateSql . NameForm Nothing <$> columnNames
    placeholders = List.intercalate "," $ map columnUpdateSql escapedColumnNames
    columnUpdateSql column = column ++ " = ?"

mkDeleteClause :: String -> String
mkDeleteClause tblName = "DELETE FROM " ++ escapedName tblName

escapedName :: String -> String
escapedName name = concat ["\"", name, "\""]
-- If you came here looking for mkSelectClause, check out
-- Database.Orville.PostgreSQL.Internal.Select
