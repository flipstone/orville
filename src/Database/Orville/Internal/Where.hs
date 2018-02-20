{-|
Module    : Database.Orville.Internal.Where
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}

{-# LANGUAGE FlexibleContexts #-}
module Database.Orville.Internal.Where where

import            Data.Convertible
import qualified  Data.List as List
import            Database.HDBC

import            Database.Orville.Internal.FieldDefinition
import            Database.Orville.Internal.Types
import            Database.Orville.Internal.QueryKey

data WhereCondition =
    BinOp String FieldDefinition SqlValue
  | IsNull FieldDefinition
  | IsNotNull FieldDefinition
  | In FieldDefinition [SqlValue]
  | NotIn FieldDefinition [SqlValue]
  | Or [WhereCondition]
  | And [WhereCondition]
  | AlwaysFalse

instance QueryKeyable WhereCondition where
  queryKey (BinOp op field value) = qkOp2 op field value
  queryKey (IsNull field) = qkOp "IS NULL" field
  queryKey (IsNotNull field) = qkOp "NOT IS NULL" field
  queryKey (In field values) = qkOp2 "IN" field values
  queryKey (NotIn field values) = qkOp2 "NOT IN" field values
  queryKey (Or conds) = qkOp "OR" conds
  queryKey (And conds) = qkOp "And" conds
  queryKey AlwaysFalse = qkOp "FALSE" QKEmpty

(.==) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .== a = BinOp "=" fieldDef (convert a)

(.<>) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .<> a = BinOp "<>" fieldDef (convert a)

(.>) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .> a = BinOp ">" fieldDef (convert a)

(.>=) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .>= a = BinOp ">=" fieldDef (convert a)

(.<) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .< a = BinOp "<" fieldDef (convert a)

(.<=) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef .<= a = BinOp "<=" fieldDef (convert a)

(.<-) :: Convertible a SqlValue
      => FieldDefinition -> [a] -> WhereCondition
_ .<- [] = AlwaysFalse
fieldDef .<- as = In fieldDef (List.nub $ map convert as)

(%==) :: Convertible a SqlValue
      => FieldDefinition -> a -> WhereCondition
fieldDef %== a = BinOp "@@" fieldDef (convert a)

whereConditionSql :: WhereCondition -> String
whereConditionSql (BinOp op fieldDef _) =
  fieldName fieldDef ++ " " ++ op ++ " ?"

whereConditionSql (IsNull fieldDef) =
  fieldName fieldDef ++ " IS NULL"

whereConditionSql (IsNotNull fieldDef) =
  fieldName fieldDef ++ " IS NOT NULL"

whereConditionSql (In fieldDef values) =
    fieldName fieldDef ++ " IN (" ++ quesses ++ ")"
  where
    quesses = List.intercalate "," (map (const "?") values)

whereConditionSql (NotIn fieldDef values) =
    fieldName fieldDef ++ " NOT IN (" ++ quesses ++ ")"
  where
    quesses = List.intercalate "," (map (const "?") values)

whereConditionSql AlwaysFalse = "TRUE = FALSE"

whereConditionSql (Or conds) = List.intercalate " OR " condsSql
  where condsSql = map condSql conds
        condSql c = "(" ++ whereConditionSql c ++ ")"

whereConditionSql (And conds) = List.intercalate " AND " condsSql
  where condsSql = map condSql conds
        condSql c = "(" ++ whereConditionSql c ++ ")"

whereConditionValues :: WhereCondition -> [SqlValue]
whereConditionValues (BinOp _ _ value) = [value]
whereConditionValues (IsNull _) = []
whereConditionValues (IsNotNull _) = []
whereConditionValues (In _ values) = values
whereConditionValues (NotIn _ values) = values
whereConditionValues AlwaysFalse = []
whereConditionValues (Or conds) = concatMap whereConditionValues conds
whereConditionValues (And conds) = concatMap whereConditionValues conds

whereAnd :: [WhereCondition] -> WhereCondition
whereAnd = And

whereOr :: [WhereCondition] -> WhereCondition
whereOr = Or

whereIn :: FieldDefinition -> [SqlValue] -> WhereCondition
whereIn = In

whereNotIn :: FieldDefinition -> [SqlValue] -> WhereCondition
whereNotIn = NotIn

isNull :: FieldDefinition -> WhereCondition
isNull fieldDef = IsNull fieldDef

isNotNull :: FieldDefinition -> WhereCondition
isNotNull fieldDef = IsNotNull fieldDef

whereClause :: [WhereCondition] -> String
whereClause [] = ""
whereClause conds = "WHERE " ++ whereConditionSql (whereAnd conds)

whereValues :: [WhereCondition] -> [SqlValue]
whereValues = List.concatMap whereConditionValues
