module Database.Orville.Internal.Sql where

import qualified  Data.List as List

mkInsertClause :: String -> [String] -> String
mkInsertClause tblName columnNames =
    "INSERT INTO " ++ escapedName tblName ++
    " (" ++ columns ++ ") VALUES (" ++ placeholders ++ ")"
  where
    columns = List.intercalate "," $ columnNames
    placeholders = List.intercalate "," $ map (const "?") columnNames

mkUpdateClause :: String -> [String] -> String
mkUpdateClause tblName columnNames =
    "UPDATE " ++ escapedName tblName ++ " SET " ++ placeholders
  where
    placeholders = List.intercalate "," $ map columnUpdateSql columnNames
    columnUpdateSql column = column ++ " = ?"

mkSelectClause :: String -> [String] -> String
mkSelectClause tblName columnNames =
    "SELECT " ++ columns ++ " FROM " ++ escapedName tblName
  where
    columns = List.intercalate ", " columnNames

mkDeleteClause :: String -> String
mkDeleteClause tblName =
  "DELETE FROM " ++ escapedName tblName

escapedName :: String -> String
escapedName name = concat ["\"", name, "\""]

