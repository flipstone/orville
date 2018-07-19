{-|
Module    : Database.Orville.Internal.Where
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.Internal.Where
  ( WhereCondition(..)
  , (.==)
  , (.<>)
  , (.>)
  , (.>=)
  , (.<)
  , (.<=)
  , (.<-)
  , (%==)
  , whereConditionValues
  , whereAnd
  , whereOr
  , whereIn
  , whereLike
  , whereLikeInsensitive
  , whereNotIn
  , whereQualified
  , isNull
  , isNotNull
  , whereClause
  , whereValues
  ) where

import qualified Data.List as List
import Database.HDBC

import qualified Database.Orville.Internal.Expr.WhereExpr as E
import Database.Orville.Internal.FieldDefinition
import Database.Orville.Internal.QueryKey
import Database.Orville.Internal.Types
import Database.Orville.Internal.Expr

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
  = WhereConditionForm E.WhereForm
  | forall a. IsNull (FieldDefinition a)
  | forall a. IsNotNull (FieldDefinition a)
  | forall a. In (FieldDefinition a)
                 [SqlValue]
  | forall a. Like (FieldDefinition a)
                   SqlValue
  | forall a. LikeInsensitive (FieldDefinition a)
                              SqlValue
  | forall a. NotIn (FieldDefinition a)
                    [SqlValue]
  | Or [WhereCondition]
  | And [WhereCondition]
  | AlwaysFalse
  | forall a b c. Qualified (TableDefinition a b c)
                            WhereCondition

instance QueryKeyable WhereCondition where
  queryKey (WhereConditionForm form) = queryKey form
  queryKey (IsNull field) = qkOp "IS NULL" field
  queryKey (IsNotNull field) = qkOp "NOT IS NULL" field
  queryKey (In field values) = qkOp2 "IN" field values
  queryKey (Like field value) = qkOp2 "LIKE" field value
  queryKey (LikeInsensitive field value) = qkOp2 "ILIKE" field value
  queryKey (NotIn field values) = qkOp2 "NOT IN" field values
  queryKey (Or conds) = qkOp "OR" conds
  queryKey (And conds) = qkOp "And" conds
  queryKey AlwaysFalse = qkOp "FALSE" QKEmpty
  queryKey (Qualified _ cond) = queryKey cond

(.==) :: FieldDefinition a -> a -> WhereCondition
fieldDef .== a = WhereConditionForm $ nameForm E..== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<> a = WhereConditionForm $ nameForm E..<> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .> a = WhereConditionForm $ nameForm E..> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .>= a = WhereConditionForm $ nameForm E..>= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<) :: FieldDefinition a -> a -> WhereCondition
fieldDef .< a = WhereConditionForm $ nameForm E..< sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<= a = WhereConditionForm $ nameForm E..<= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<-) :: FieldDefinition a -> [a] -> WhereCondition
_ .<- [] = AlwaysFalse
fieldDef .<- as = In fieldDef (List.nub $ map (fieldToSqlValue fieldDef) as)

(%==) :: FieldDefinition a -> a -> WhereCondition
fieldDef %== a = WhereConditionForm $ nameForm E.%== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

whereConditionSql :: WhereCondition -> String
whereConditionSql cond = internalWhereConditionSql Nothing cond

internalWhereConditionSql ::
     Maybe (TableDefinition a b c) -> WhereCondition -> String
internalWhereConditionSql (Just tableDef) (WhereConditionForm form) =
  rawExprToSql . generateSql $ form `qualified` (tableName tableDef)
internalWhereConditionSql Nothing (WhereConditionForm form) =
  rawExprToSql . generateSql $ form
internalWhereConditionSql tableDef (IsNull fieldDef) =
  qualifiedFieldName tableDef fieldDef ++ " IS NULL"
internalWhereConditionSql tableDef (IsNotNull fieldDef) =
  qualifiedFieldName tableDef fieldDef ++ " IS NOT NULL"
internalWhereConditionSql tableDef (In fieldDef values) =
  qualifiedFieldName tableDef fieldDef ++ " IN (" ++ quesses ++ ")"
  where
    quesses = List.intercalate "," (map (const "?") values)
internalWhereConditionSql tableDef (Like fieldDef _) =
  qualifiedFieldName tableDef fieldDef ++ " LIKE ?"
internalWhereConditionSql tableDef (LikeInsensitive fieldDef _) =
  qualifiedFieldName tableDef fieldDef ++ " ILIKE ?"
internalWhereConditionSql tableDef (NotIn fieldDef values) =
  qualifiedFieldName tableDef fieldDef ++ " NOT IN (" ++ quesses ++ ")"
  where
    quesses = List.intercalate "," (map (const "?") values)
internalWhereConditionSql _ AlwaysFalse = "TRUE = FALSE"
internalWhereConditionSql tableDef (Or conds) =
  List.intercalate " OR " condsSql
  where
    condsSql = map condSql conds
    condSql c = "(" ++ internalWhereConditionSql tableDef c ++ ")"
internalWhereConditionSql tableDef (And conds) =
  List.intercalate " AND " condsSql
  where
    condsSql = map condSql conds
    condSql c = "(" ++ internalWhereConditionSql tableDef c ++ ")"
internalWhereConditionSql _ (Qualified tableDef cond) =
  internalWhereConditionSql (Just tableDef) cond

qualifiedFieldName ::
     Maybe (TableDefinition a b c) -> FieldDefinition d -> String
qualifiedFieldName maybeTableDef fieldDef =
  case maybeTableDef of
    Just tableDef -> tableName tableDef ++ "." ++ fieldName fieldDef
    Nothing -> fieldName fieldDef

whereConditionValues :: WhereCondition -> [SqlValue]
whereConditionValues (WhereConditionForm form) = E.whereValues [form]
whereConditionValues (IsNull _) = []
whereConditionValues (IsNotNull _) = []
whereConditionValues (In _ values) = values
whereConditionValues (Like _ value) = [value]
whereConditionValues (LikeInsensitive _ value) = [value]
whereConditionValues (NotIn _ values) = values
whereConditionValues AlwaysFalse = []
whereConditionValues (Or conds) = concatMap whereConditionValues conds
whereConditionValues (And conds) = concatMap whereConditionValues conds
whereConditionValues (Qualified _ cond) = whereConditionValues cond

whereAnd :: [WhereCondition] -> WhereCondition
whereAnd = And

whereOr :: [WhereCondition] -> WhereCondition
whereOr = Or

whereIn :: FieldDefinition a -> [a] -> WhereCondition
whereIn fieldDef values = In fieldDef (map (fieldToSqlValue fieldDef) values)

whereLike :: FieldDefinition a -> String -> WhereCondition
whereLike fieldDef raw = Like fieldDef (toSql raw)

whereLikeInsensitive :: FieldDefinition a -> String -> WhereCondition
whereLikeInsensitive fieldDef raw = LikeInsensitive fieldDef (toSql raw)

whereNotIn :: FieldDefinition a -> [a] -> WhereCondition
whereNotIn fieldDef values =
  NotIn fieldDef (map (fieldToSqlValue fieldDef) values)

whereQualified :: TableDefinition a b c -> WhereCondition -> WhereCondition
whereQualified tableDef cond = Qualified tableDef cond

isNull :: FieldDefinition a -> WhereCondition
isNull fieldDef = IsNull fieldDef

isNotNull :: FieldDefinition a -> WhereCondition
isNotNull fieldDef = IsNotNull fieldDef

whereClause :: [WhereCondition] -> String
whereClause [] = ""
whereClause conds = "WHERE " ++ whereConditionSql (whereAnd conds)

whereValues :: [WhereCondition] -> [SqlValue]
whereValues = List.concatMap whereConditionValues
