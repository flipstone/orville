module Database.Orville.Internal.Expr.WhereExpr
( WhereExpr
, WhereForm
, (.==)
, (.<>)
, (.>)
, (.>=)
, (.<)
, (.<=)
, (%==)
, whereValues
, whereNull
, whereNotNull
, whereRaw
) where

import qualified Data.List as List
import Data.Monoid
import Database.HDBC

import Database.Orville.Internal.Expr.Expr
import Database.Orville.Internal.Expr.NameExpr
import Database.Orville.Internal.QueryKey

type WhereExpr = Expr WhereForm

data WhereForm
  = WhereBinOp String NameForm SqlValue
  | WhereNull NameForm
  | WhereNotNull NameForm
  | WhereRaw String [SqlValue]

instance QualifySql WhereForm where
  qualified (WhereBinOp op field value) table =
    WhereBinOp op (field `qualified` table) value
  qualified (WhereNull field) table =
    WhereNull (field `qualified` table)
  qualified (WhereNotNull field) table =
    WhereNotNull (field `qualified` table)
  qualified raw@(WhereRaw _ _) _ = raw

instance QueryKeyable WhereForm where
  queryKey (WhereBinOp op field value) = qkOp2 op field value
  queryKey (WhereNull field) = qkOp "IS NULL" field
  queryKey (WhereNotNull field) = qkOp "NOT IS NULL" field
  queryKey (WhereRaw raw values) = qkOp raw values

instance GenerateSql WhereForm where
  generateSql (WhereBinOp op field _) =
    (generateSql field) <> rawSql (" " <> op <> " ?")
  generateSql (WhereNull field) =
    (generateSql field) <> rawSql " IS NULL"
  generateSql (WhereNotNull field) =
    (generateSql field) <> rawSql " IS NOT NULL"
  generateSql (WhereRaw raw _) = rawSql raw

(.==) :: NameForm -> SqlValue -> WhereForm
name .== value = WhereBinOp "=" name value

(.<>) :: NameForm -> SqlValue -> WhereForm
name .<> value = WhereBinOp "<>" name value

(.>) :: NameForm -> SqlValue -> WhereForm
name .> value = WhereBinOp ">" name value

(.>=) :: NameForm -> SqlValue -> WhereForm
name .>= value = WhereBinOp ">=" name value

(.<) :: NameForm -> SqlValue -> WhereForm
name .< value = WhereBinOp "<" name value

(.<=) :: NameForm -> SqlValue -> WhereForm
name .<= value = WhereBinOp "<=" name value

(%==) :: NameForm -> SqlValue -> WhereForm
name %== value = WhereBinOp "@@" name value

whereNull :: NameForm -> WhereForm
whereNull = WhereNull

whereNotNull :: NameForm -> WhereForm
whereNotNull = WhereNotNull

whereRaw :: String -> [SqlValue] -> WhereForm
whereRaw = WhereRaw

whereValues :: [WhereForm] -> [SqlValue]
whereValues = List.concatMap whereValuesInternal

whereValuesInternal :: WhereForm -> [SqlValue]
whereValuesInternal (WhereBinOp _ _ value) = [value]
whereValuesInternal (WhereNull _) = []
whereValuesInternal (WhereNotNull _) = []
whereValuesInternal (WhereRaw _ values) = values
