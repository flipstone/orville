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

instance QualifySql WhereForm where
  qualified (WhereBinOp op name value) table =
    WhereBinOp op (name `qualified` table) value

instance QueryKeyable WhereForm where
  queryKey (WhereBinOp op field value) = qkOp2 op field value

instance GenerateSql WhereForm where
  generateSql (WhereBinOp op name _) =
    (generateSql name) <> rawSql (" " <> op <> " ?")

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

whereValues :: [WhereForm] -> [SqlValue]
whereValues = List.concatMap whereValuesInternal

whereValuesInternal :: WhereForm -> [SqlValue]
whereValuesInternal (WhereBinOp _ _ value) = [value]
