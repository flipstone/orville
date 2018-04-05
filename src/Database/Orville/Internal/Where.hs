{-|
Module    : Database.Orville.Internal.Where
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.Internal.Where where

import qualified Data.List as List
import Database.HDBC

import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.QueryKey
import Database.Orville.Internal.Types

{-
  It would be nice to match the SqlValues in these with the types from the
  corresponding FieldDefinitions. However, this would require adding an
  Eq constraint for List.nub on the .<- operation, which I'm not willing
  to do at this moment.

  Alternately, we could eliminate storing the entire FieldDefinition here,
  thereby removing the need for ExistentialQuantification. Currently the
  field definition is being used in the QueryKeyable instances for WhereCondition
  for calls to qkOp an friends. Replacing FieldDefinition with just the field
  name here would be nice. We would probably want a fully-fledged FieldName
  type whech could provide the appropriate QueryKeyable instance. That then
  raises questions about the ergonomics users creating FieldDefinition values
  without requiring OverloadedStrings to be turned on.
-}
data WhereCondition
  = forall a. BinOp String
                    (FieldDefinition a)
                    SqlValue
  | forall a. IsNull (FieldDefinition a)
  | forall a. IsNotNull (FieldDefinition a)
  | forall a. In (FieldDefinition a)
                 [SqlValue]
  | forall a. NotIn (FieldDefinition a)
                    [SqlValue]
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

(.==) :: FieldDefinition a -> a -> WhereCondition
fieldDef .== a = BinOp "=" fieldDef (fieldToSqlValue fieldDef a)

(.<>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<> a = BinOp "<>" fieldDef (fieldToSqlValue fieldDef a)

(.>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .> a = BinOp ">" fieldDef (fieldToSqlValue fieldDef a)

(.>=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .>= a = BinOp ">=" fieldDef (fieldToSqlValue fieldDef a)

(.<) :: FieldDefinition a -> a -> WhereCondition
fieldDef .< a = BinOp "<" fieldDef (fieldToSqlValue fieldDef a)

(.<=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<= a = BinOp "<=" fieldDef (fieldToSqlValue fieldDef a)

(.<-) :: FieldDefinition a -> [a] -> WhereCondition
_ .<- [] = AlwaysFalse
fieldDef .<- as = In fieldDef (List.nub $ map (fieldToSqlValue fieldDef) as)

(%==) :: FieldDefinition a -> a -> WhereCondition
fieldDef %== a = BinOp "@@" fieldDef (fieldToSqlValue fieldDef a)

whereConditionSql :: WhereCondition -> String
whereConditionSql (BinOp op fieldDef _) =
  fieldName fieldDef ++ " " ++ op ++ " ?"
whereConditionSql (IsNull fieldDef) = fieldName fieldDef ++ " IS NULL"
whereConditionSql (IsNotNull fieldDef) = fieldName fieldDef ++ " IS NOT NULL"
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
  where
    condsSql = map condSql conds
    condSql c = "(" ++ whereConditionSql c ++ ")"
whereConditionSql (And conds) = List.intercalate " AND " condsSql
  where
    condsSql = map condSql conds
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

whereIn :: FieldDefinition a -> [a] -> WhereCondition
whereIn fieldDef values = In fieldDef (map (fieldToSqlValue fieldDef) values)

whereNotIn :: FieldDefinition a -> [a] -> WhereCondition
whereNotIn fieldDef values =
  NotIn fieldDef (map (fieldToSqlValue fieldDef) values)

isNull :: FieldDefinition a -> WhereCondition
isNull fieldDef = IsNull fieldDef

isNotNull :: FieldDefinition a -> WhereCondition
isNotNull fieldDef = IsNotNull fieldDef

whereClause :: [WhereCondition] -> String
whereClause [] = ""
whereClause conds = "WHERE " ++ whereConditionSql (whereAnd conds)

whereValues :: [WhereCondition] -> [SqlValue]
whereValues = List.concatMap whereConditionValues
