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
import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.QueryKey
import Database.Orville.Internal.Types

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

(.==) :: FieldDefinition a -> a -> WhereForm
field .== a = whereBinOp "=" field a

(.<>) :: FieldDefinition a -> a -> WhereForm
field .<> a = whereBinOp "<>" field a

(.>) :: FieldDefinition a -> a -> WhereForm
field .> a = whereBinOp ">" field a

(.>=) :: FieldDefinition a -> a -> WhereForm
field .>= a = whereBinOp ">=" field a

(.<) :: FieldDefinition a -> a -> WhereForm
field .< a = whereBinOp "<" field a

(.<=) :: FieldDefinition a -> a -> WhereForm
field .<= a = whereBinOp "<=" field a

(%==) :: FieldDefinition a -> a -> WhereForm
field %== a = whereBinOp "@@" field a

whereBinOp :: String -> FieldDefinition a -> a -> WhereForm
whereBinOp op field a = WhereBinOp op nameForm sqlValue
  where
    nameForm = fieldToNameForm field
    sqlValue = (fieldToSqlValue field a)

whereValues :: [WhereForm] -> [SqlValue]
whereValues = List.concatMap whereValues'

whereValues' :: WhereForm -> [SqlValue]
whereValues' (WhereBinOp _ _ value) = [value]
