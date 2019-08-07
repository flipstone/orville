{-|
Module    : Database.Orville.Oracle.Internal.Select
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
module Database.Orville.Oracle.Internal.Select where

import Control.Monad.Reader
import qualified Data.List as List
import Database.HDBC

import Database.Orville.Oracle.Internal.Expr
import Database.Orville.Oracle.Internal.FieldDefinition (fieldToNameForm)
import Database.Orville.Oracle.Internal.FromClause
import Database.Orville.Oracle.Internal.SelectOptions
import Database.Orville.Oracle.Internal.Types

data Select row = Select
  { selectBuilder :: FromSql row
  , selectSql :: String
  , selectValues :: [SqlValue]
  }

selectQueryColumns ::
     [SelectExpr] -> FromSql row -> FromClause -> SelectOptions -> Select row
selectQueryColumns selectExprs builder fromClause opts =
  selectQueryRaw builder querySql (selectOptValues opts)
  where
    columns =
      List.intercalate ", " $ map (rawExprToSql . generateSql) selectExprs
    querySql =
      List.concat
        [ selectClause opts
        , columns
        , " "
        , fromClauseToSql fromClause
        , " "
        , selectOptClause opts
        ]

selectQuery :: FromSql row -> FromClause -> SelectOptions -> Select row
selectQuery builder =
  selectQueryColumns (expr <$> fromSqlSelects builder) builder

selectQueryTable ::
     TableDefinition readEntity writeEntity key
  -> SelectOptions
  -> Select readEntity
selectQueryTable tbl = selectQuery (tableFromSql tbl) (fromClauseTable tbl)

selectQueryRows ::
     [SelectExpr] -> FromClause -> SelectOptions -> Select [(String, SqlValue)]
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
rowFromSql =
  FromSql
    { fromSqlSelects =
        error
          "Database.Orville.Oracle.Select.rowFromSql: fromSqlColumnNames was accessed. This is a bug."
    , runFromSql = Right <$> ask
    }

selectField :: FieldDefinition a -> SelectForm
selectField field = selectColumn (fieldToNameForm field)
