{-|
Module    : Database.Orville.PostgreSQL.Internal.Sql
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.PostgreSQL.Internal.Sql where

import qualified Data.List as List

mkInsertClause :: String -> [String] -> String
mkInsertClause tblName columnNames =
  mkInsertIntoClause tblName columnNames ++ " VALUES (" ++ placeholders ++ ")"
  where
    placeholders = List.intercalate "," $ map (const "?") columnNames

mkInsertManyClause :: String -> [String] -> Int -> String
mkInsertManyClause tblName columnNames recordCount =
  mkInsertIntoClause tblName columnNames ++ " VALUES " ++ placeholders
  where
    placeholder = "(" ++ (List.intercalate "," $ map (const "?") columnNames) ++ ")"
    placeholders = List.intercalate "," $ replicate recordCount placeholder

mkInsertIntoClause :: String -> [String] -> String
mkInsertIntoClause tblName columnNames =
  "INSERT INTO " ++
  escapedName tblName ++ " (" ++ columns ++ ")"
  where
    columns = List.intercalate "," $ columnNames

mkUpdateClause :: String -> [String] -> String
mkUpdateClause tblName columnNames =
  "UPDATE " ++ escapedName tblName ++ " SET " ++ placeholders
  where
    placeholders = List.intercalate "," $ map columnUpdateSql columnNames
    columnUpdateSql column = column ++ " = ?"

mkDeleteClause :: String -> String
mkDeleteClause tblName = "DELETE FROM " ++ escapedName tblName

escapedName :: String -> String
escapedName name = concat ["\"", name, "\""]
-- If you came here looking for mkSelectClause, check out
-- Database.Orville.PostgreSQL.Internal.Select
