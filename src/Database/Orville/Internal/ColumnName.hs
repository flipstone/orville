module Database.Orville.Internal.ColumnName where

import            Database.Orville.Internal.Sql

newtype ColumnName = ColumnName String

columnNameToSql :: ColumnName -> String
columnNameToSql (ColumnName sql) = sql

columnNameRaw :: String -> ColumnName
columnNameRaw = ColumnName

columnNameSimple :: String -> ColumnName
columnNameSimple = columnNameRaw . escapedName

columnNameQualified :: String -> String -> ColumnName
columnNameQualified source column =
  columnNameRaw $ concat $
    [ escapedName source
    , "."
    , escapedName column
    ]

