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
, whereLike
, whereLikeInsensitive
, whereNull
, whereNotNull
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
  | WhereLike NameForm SqlValue
  | WhereLikeInsensitive NameForm SqlValue
  | WhereNull NameForm
  | WhereNotNull NameForm

instance QualifySql WhereForm where
  qualified (WhereBinOp op field value) table =
    WhereBinOp op (field `qualified` table) value
  qualified (WhereLike field value) table =
    WhereLike (field `qualified` table) value
  qualified (WhereLikeInsensitive field value) table =
    WhereLikeInsensitive (field `qualified` table) value
  qualified (WhereNull field) table =
    WhereNull (field `qualified` table)
  qualified (WhereNotNull field) table =
    WhereNotNull (field `qualified` table)

instance QueryKeyable WhereForm where
  queryKey (WhereBinOp op field value) = qkOp2 op field value
  queryKey (WhereLike field value) = qkOp2 "LIKE" field value
  queryKey (WhereLikeInsensitive field value) = qkOp2 "ILIKE" field value
  queryKey (WhereNull field) = qkOp "IS NULL" field
  queryKey (WhereNotNull field) = qkOp "NOT IS NULL" field

instance GenerateSql WhereForm where
  generateSql (WhereBinOp op field _) =
    (generateSql field) <> rawSql (" " <> op <> " ?")
  generateSql (WhereLike field _) =
    (generateSql field) <> rawSql " LIKE ?"
  generateSql (WhereLikeInsensitive field _) =
    (generateSql field) <> rawSql " ILIKE ?"
  generateSql (WhereNull field) =
    (generateSql field) <> rawSql " IS NULL"
  generateSql (WhereNotNull field) =
    (generateSql field) <> rawSql " IS NOT NULL"

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

whereLike :: NameForm -> SqlValue -> WhereForm
whereLike = WhereLike

whereLikeInsensitive :: NameForm -> SqlValue -> WhereForm
whereLikeInsensitive = WhereLikeInsensitive

whereNull :: NameForm -> WhereForm
whereNull = WhereNull

whereNotNull :: NameForm -> WhereForm
whereNotNull = WhereNotNull

whereValues :: [WhereForm] -> [SqlValue]
whereValues = List.concatMap whereValuesInternal

whereValuesInternal :: WhereForm -> [SqlValue]
whereValuesInternal (WhereBinOp _ _ value) = [value]
whereValuesInternal (WhereLike _ value) = [value]
whereValuesInternal (WhereLikeInsensitive _ value) = [value]
whereValuesInternal (WhereNull _) = []
whereValuesInternal (WhereNotNull _) = []
