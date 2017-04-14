module Database.Orville.Internal.Select where

import            Control.Monad.Reader
import qualified  Data.List as List
import            Database.HDBC

import            Database.Orville.Internal.Expr
import            Database.Orville.Internal.FromClause
import            Database.Orville.Internal.SelectOptions
import            Database.Orville.Internal.Types
import            Database.Orville.Internal.FieldDefinition (fieldName)


data Select row = Select
  { selectBuilder :: FromSql row
  , selectSql :: String
  , selectValues :: [SqlValue]
  }

selectQueryColumns :: [SelectExpr] -> FromSql row -> FromClause -> SelectOptions -> Select row
selectQueryColumns selectExprs builder fromClause opts =
    selectQueryRaw builder querySql (selectOptValues opts)
  where
    columns = List.intercalate ", " $ map (rawExprToSql . generateSql) selectExprs
    querySql = List.concat [ "SELECT "
                           , columns
                           , " "
                           , fromClauseToSql fromClause
                           , " "
                           , selectOptClause opts
                           ]

selectQuery :: FromSql row -> FromClause -> SelectOptions -> Select row
selectQuery builder = selectQueryColumns (expr <$> fromSqlSelects builder) builder

selectQueryTable :: TableDefinition entity -> SelectOptions -> Select (entity Record)
selectQueryTable tbl = selectQuery (tableFromSql tbl) (fromClauseTable tbl)

selectQueryRows :: [SelectExpr] -> FromClause -> SelectOptions -> Select [(String, SqlValue)]
selectQueryRows exprs = selectQueryColumns exprs rowFromSql

selectQueryRaw :: FromSql row -> String -> [SqlValue] -> Select row
selectQueryRaw = Select

selectQueryRawRows :: String -> [SqlValue] -> Select [(String, SqlValue)]
selectQueryRawRows = selectQueryRaw rowFromSql

-- N.B. This FromSql does *not* contain an accurate list of the columns
-- it decodes, because it does not decode any columns at all. It is not
-- suitable for uses where the FromSql is used to generate the columns in
-- a select clause. It is not exposed publically for this reason.
--
rowFromSql :: FromSql [(String, SqlValue)]
rowFromSql = FromSql
  { fromSqlSelects = error "Database.Orville.Select.rowFromSql: fromSqlColumnNames was accessed. This is a bug."
  , runFromSql = Right <$> ask
  }

selectField :: FieldDefinition -> SelectForm
selectField field = selectColumn (NameForm (fieldName field))

selectFieldAs :: FieldDefinition -> String -> SelectForm
selectFieldAs field alias = aliased selForm (NameForm alias)
  where selForm = selectField field

selectCustomValue :: String -> SelectForm
selectCustomValue s = selectColumn (NameForm s)
