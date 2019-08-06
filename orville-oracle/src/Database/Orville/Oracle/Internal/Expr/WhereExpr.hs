module Database.Orville.Oracle.Internal.Expr.WhereExpr
  ( WhereExpr
  , WhereForm
  , (.==)
  , (.<>)
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , (%==)
  , (.<-)
  , whereValues
  , whereIn
  , whereNotIn
  , whereLike
  , whereLikeInsensitive
  , whereNull
  , whereNotNull
  , whereRaw
  ) where

import qualified Data.List as List

import Database.HDBC

import Database.Orville.Oracle.Internal.MappendCompat ((<>))

import Database.Orville.Oracle.Internal.Expr.Expr
import Database.Orville.Oracle.Internal.Expr.NameExpr
import Database.Orville.Oracle.Internal.QueryKey

type WhereExpr = Expr WhereForm

data WhereForm
  = WhereAlwaysFalse
  | WhereAlwaysTrue
  | WhereBinOp String
               NameForm
               SqlValue
  | WhereIn NameForm
            [SqlValue]
  | WhereNotIn NameForm
               [SqlValue]
  | WhereLike NameForm
              SqlValue
  | WhereLikeInsensitive NameForm
                         SqlValue
  | WhereNull NameForm
  | WhereNotNull NameForm
  | WhereRaw String
             [SqlValue]

instance QualifySql WhereForm where
  qualified cond@WhereAlwaysFalse _ = cond
  qualified cond@WhereAlwaysTrue _ = cond
  qualified (WhereBinOp op field value) table =
    WhereBinOp op (field `qualified` table) value
  qualified (WhereIn field values) table =
    WhereIn (field `qualified` table) values
  qualified (WhereNotIn field values) table =
    WhereNotIn (field `qualified` table) values
  qualified (WhereLike field value) table =
    WhereLike (field `qualified` table) value
  qualified (WhereLikeInsensitive field value) table =
    WhereLikeInsensitive (field `qualified` table) value
  qualified (WhereNull field) table = WhereNull (field `qualified` table)
  qualified (WhereNotNull field) table = WhereNotNull (field `qualified` table)
  qualified raw@(WhereRaw _ _) _ = raw

instance QueryKeyable WhereForm where
  queryKey WhereAlwaysFalse = qkOp "FALSE" QKEmpty
  queryKey WhereAlwaysTrue = qkOp "TRUE" QKEmpty
  queryKey (WhereBinOp op field value) = qkOp2 op field value
  queryKey (WhereIn field values) = qkOp2 "IN" field values
  queryKey (WhereNotIn field values) = qkOp2 "NOT IN" field values
  queryKey (WhereLike field value) = qkOp2 "LIKE" field value
  queryKey (WhereLikeInsensitive field value) = qkOp2 "ILIKE" field value
  queryKey (WhereNull field) = qkOp "IS NULL" field
  queryKey (WhereNotNull field) = qkOp "NOT IS NULL" field
  queryKey (WhereRaw raw values) = qkOp raw values

instance GenerateSql WhereForm where
  generateSql WhereAlwaysFalse = rawSql "TRUE = FALSE"
  generateSql WhereAlwaysTrue = rawSql "TRUE = TRUE"
  generateSql (WhereBinOp op field _) =
    (generateSql field) <> rawSql (" " <> op <> " ?")
  generateSql (WhereIn field values) =
    (generateSql field) <> rawSql (" IN (" <> quesses <> ")")
    where
      quesses = List.intercalate "," (map (const "?") values)
  generateSql (WhereNotIn field values) =
    (generateSql field) <> rawSql (" NOT IN (" <> quesses <> ")")
    where
      quesses = List.intercalate "," (map (const "?") values)
  generateSql (WhereLike field _) = (generateSql field) <> rawSql " LIKE ?"
  generateSql (WhereLikeInsensitive field _) =
    (generateSql field) <> rawSql " ILIKE ?"
  generateSql (WhereNull field) = (generateSql field) <> rawSql " IS NULL"
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

(.<-) :: NameForm -> [SqlValue] -> WhereForm
name .<- values = whereIn name values

whereIn :: NameForm -> [SqlValue] -> WhereForm
whereIn _ [] = WhereAlwaysFalse
whereIn field values = WhereIn field (List.nub values)

whereNotIn :: NameForm -> [SqlValue] -> WhereForm
whereNotIn _ [] = WhereAlwaysTrue
whereNotIn field values = WhereNotIn field (List.nub values)

whereLike :: NameForm -> SqlValue -> WhereForm
whereLike = WhereLike

whereLikeInsensitive :: NameForm -> SqlValue -> WhereForm
whereLikeInsensitive = WhereLikeInsensitive

whereNull :: NameForm -> WhereForm
whereNull = WhereNull

whereNotNull :: NameForm -> WhereForm
whereNotNull = WhereNotNull

whereRaw :: String -> [SqlValue] -> WhereForm
whereRaw = WhereRaw

whereValues :: [WhereForm] -> [SqlValue]
whereValues = List.concatMap whereValuesInternal

whereValuesInternal :: WhereForm -> [SqlValue]
whereValuesInternal (WhereAlwaysFalse) = []
whereValuesInternal (WhereAlwaysTrue) = []
whereValuesInternal (WhereBinOp _ _ value) = [value]
whereValuesInternal (WhereIn _ values) = values
whereValuesInternal (WhereNotIn _ values) = values
whereValuesInternal (WhereLike _ value) = [value]
whereValuesInternal (WhereLikeInsensitive _ value) = [value]
whereValuesInternal (WhereNull _) = []
whereValuesInternal (WhereNotNull _) = []
whereValuesInternal (WhereRaw _ values) = values
