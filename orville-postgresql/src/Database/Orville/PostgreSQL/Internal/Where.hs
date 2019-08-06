{-|
Module    : Database.Orville.PostgreSQL.Internal.Where
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Orville.PostgreSQL.Internal.Where
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
  , whereRaw
  , isNull
  , isNotNull
  , whereClause
  , whereValues
  ) where

import qualified Data.List as List
import Database.HDBC

import Database.Orville.PostgreSQL.Internal.Expr
import qualified Database.Orville.PostgreSQL.Internal.Expr.WhereExpr as E
import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.Types

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
  = WhereConditionExpr E.WhereExpr
  | Or [WhereCondition]
  | And [WhereCondition]
  | forall a b c. Qualified (TableDefinition a b c)
                            WhereCondition

instance QueryKeyable WhereCondition where
  queryKey (WhereConditionExpr (Expr (Right form))) = queryKey form
  queryKey (WhereConditionExpr (Expr (Left raw))) = QKField $ rawExprToSql raw
  queryKey (Or conds) = qkOp "OR" conds
  queryKey (And conds) = qkOp "And" conds
  queryKey (Qualified _ cond) = queryKey cond

(.==) :: FieldDefinition a -> a -> WhereCondition
fieldDef .== a = WhereConditionExpr . expr $ nameForm E..== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<> a = WhereConditionExpr . expr $ nameForm E..<> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>) :: FieldDefinition a -> a -> WhereCondition
fieldDef .> a = WhereConditionExpr . expr $ nameForm E..> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .>= a = WhereConditionExpr . expr $ nameForm E..>= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<) :: FieldDefinition a -> a -> WhereCondition
fieldDef .< a = WhereConditionExpr . expr $ nameForm E..< sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<=) :: FieldDefinition a -> a -> WhereCondition
fieldDef .<= a = WhereConditionExpr . expr $ nameForm E..<= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<-) :: FieldDefinition a -> [a] -> WhereCondition
fieldDef .<- as = whereIn fieldDef as

(%==) :: FieldDefinition a -> a -> WhereCondition
fieldDef %== a = WhereConditionExpr . expr $ nameForm E.%== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

whereConditionSql :: WhereCondition -> String
whereConditionSql cond = internalWhereConditionSql Nothing cond

internalWhereConditionSql ::
     Maybe (TableDefinition a b c) -> WhereCondition -> String
internalWhereConditionSql (Just tableDef) (WhereConditionExpr expression) =
  rawExprToSql . generateSql $ expression `qualified` (tableName tableDef)
internalWhereConditionSql Nothing (WhereConditionExpr expression) =
  rawExprToSql . generateSql $ expression
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

whereConditionValues :: WhereCondition -> [SqlValue]
whereConditionValues (WhereConditionExpr (Expr (Right form))) =
  E.whereValues [form]
whereConditionValues (WhereConditionExpr (Expr (Left _))) = []
whereConditionValues (Or conds) = concatMap whereConditionValues conds
whereConditionValues (And conds) = concatMap whereConditionValues conds
whereConditionValues (Qualified _ cond) = whereConditionValues cond

whereAnd :: [WhereCondition] -> WhereCondition
whereAnd = And

whereOr :: [WhereCondition] -> WhereCondition
whereOr = Or

whereIn :: FieldDefinition a -> [a] -> WhereCondition
whereIn fieldDef values =
  WhereConditionExpr . expr $
  E.whereIn (fieldToNameForm fieldDef) (map (fieldToSqlValue fieldDef) values)

whereLike :: FieldDefinition a -> String -> WhereCondition
whereLike fieldDef raw =
  WhereConditionExpr . expr $
  E.whereLike (fieldToNameForm fieldDef) (toSql raw)

whereLikeInsensitive :: FieldDefinition a -> String -> WhereCondition
whereLikeInsensitive fieldDef raw =
  WhereConditionExpr . expr $
  E.whereLikeInsensitive (fieldToNameForm fieldDef) (toSql raw)

whereNotIn :: FieldDefinition a -> [a] -> WhereCondition
whereNotIn fieldDef values =
  WhereConditionExpr . expr $
  E.whereNotIn
    (fieldToNameForm fieldDef)
    (map (fieldToSqlValue fieldDef) values)

whereQualified :: TableDefinition a b c -> WhereCondition -> WhereCondition
whereQualified tableDef cond = Qualified tableDef cond

whereRaw :: String -> [SqlValue] -> WhereCondition
whereRaw str values = WhereConditionExpr . expr $ E.whereRaw str values

isNull :: FieldDefinition a -> WhereCondition
isNull = WhereConditionExpr . expr . E.whereNull . fieldToNameForm

isNotNull :: FieldDefinition a -> WhereCondition
isNotNull = WhereConditionExpr . expr . E.whereNotNull . fieldToNameForm

whereClause :: [WhereCondition] -> String
whereClause [] = ""
whereClause conds = "WHERE " ++ whereConditionSql (whereAnd conds)

whereValues :: [WhereCondition] -> [SqlValue]
whereValues = List.concatMap whereConditionValues
