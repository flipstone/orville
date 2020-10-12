{-|
Module    : Database.Orville.Oracle.Internal.Sql
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Internal.Sql where

import qualified Data.List as List

import Data.String.Helpers(escapeString)
import Database.Orville.Oracle.Internal.Expr

mkInsertClause :: String -> [String] -> String
mkInsertClause tblName columnNames =
  "INSERT INTO " ++
  tblName ++ " (" ++ columns ++ ") VALUES (" ++ placeholders ++ ")"
  where
    escapedColumnNames = rawExprToSql . generateSql . NameForm Nothing <$> columnNames
    columns = List.intercalate "," escapedColumnNames
    placeholders = List.intercalate "," $ map (const "?") columnNames

mkUpdateClause :: String -> [String] -> String
mkUpdateClause tblName columnNames =
  "UPDATE " <> escapeString tblName <> " SET " <> placeholders
  where
    escapedColumnNames = rawExprToSql . generateSql . NameForm Nothing <$> columnNames
    placeholders = List.intercalate "," $ map columnUpdateSql escapedColumnNames
    columnUpdateSql column = column <> " = ?"

mkDeleteClause :: String -> String
mkDeleteClause tblName = "DELETE FROM " <> escapeString tblName

-- If you came here looking for mkSelectClause, check out
-- Database.Orville.Oracle.Internal.Select
