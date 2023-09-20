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
  , whereToSql
  ) where

import qualified Data.List as List
import Database.HDBC

import Database.Orville.PostgreSQL.Internal.Expr
import qualified Database.Orville.PostgreSQL.Internal.Expr.WhereExpr as E
import Database.Orville.PostgreSQL.Internal.FieldDefinition
import Database.Orville.PostgreSQL.Internal.QueryKey
import Database.Orville.PostgreSQL.Internal.Types

{-
  Migration Guide: @WhereCondition@ has been replaced with @BooleanExpr@

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

(.==) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .== a = WhereConditionExpr . expr $ nameForm E..== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<>) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .<> a = WhereConditionExpr . expr $ nameForm E..<> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .> a = WhereConditionExpr . expr $ nameForm E..> sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.>=) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .>= a = WhereConditionExpr . expr $ nameForm E..>= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .< a = WhereConditionExpr . expr $ nameForm E..< sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<=) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef .<= a = WhereConditionExpr . expr $ nameForm E..<= sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

(.<-) :: FieldDefinition nullability a -> [a] -> WhereCondition
fieldDef .<- as = whereIn fieldDef as

(%==) :: FieldDefinition nullability a -> a -> WhereCondition
fieldDef %== a = WhereConditionExpr . expr $ nameForm E.%== sqlValue
  where
    nameForm = fieldToNameForm fieldDef
    sqlValue = fieldToSqlValue fieldDef a

whereConditionSql :: WhereCondition -> String
whereConditionSql cond = internalWhereConditionSql Nothing cond

internalWhereConditionSql ::
     Maybe (TableDefinition a b c) -> WhereCondition -> String
internalWhereConditionSql mbTableDef whereCondition =
  case whereCondition of
    Or [] -> "FALSE"
    Or conds ->
      let condsSql = map innerCondSql conds
       in List.intercalate " OR " condsSql

    And [] -> "TRUE"
    And conds ->
      let condsSql = map innerCondSql conds
       in List.intercalate " AND " condsSql

    WhereConditionExpr expression ->
      case mbTableDef of
        Just tableDef ->
          rawExprToSql . generateSql $ expression `qualified` (tableName tableDef)
        Nothing ->
          rawExprToSql . generateSql $ expression

    Qualified tableDef cond ->
      internalWhereConditionSql (Just tableDef) cond
  where
    innerCondSql c =
      let sql = internalWhereConditionSql mbTableDef c
       in "(" ++ sql ++ ")"

whereConditionValues :: WhereCondition -> [SqlValue]
whereConditionValues (WhereConditionExpr (Expr (Right form))) =
  E.whereValues [form]
whereConditionValues (WhereConditionExpr (Expr (Left _))) = []
whereConditionValues (Or conds) = concatMap whereConditionValues conds
whereConditionValues (And conds) = concatMap whereConditionValues conds
whereConditionValues (Qualified _ cond) = whereConditionValues cond

{- |
  Migration Guide: @whereAnd@ has been removed. Use the binary function
  @andExpr@ to combine @BooleanExpr@ expressions instead. @andExpr@ is also
  available as the operator @(.&&)@
-}
whereAnd :: [WhereCondition] -> WhereCondition
whereAnd = And

{- |
  Migration Guide: @whereOr@ has been removed. Use the binary function
  @orExpr@ to combine @BooleanExpr@ expressions instead. @orExpr@ is also
  available as the operator @(.||)@
-}
whereOr :: [WhereCondition] -> WhereCondition
whereOr = Or

{- |
  Migration Guide: @whereIn@ has renamed to @fieldIn@. It now takes a
  @NonEmpty@ list of values to reflect this is a requirement in SQL.
-}
whereIn :: FieldDefinition nullability a -> [a] -> WhereCondition
whereIn fieldDef values =
  WhereConditionExpr . expr $
  E.whereIn (fieldToNameForm fieldDef) (map (fieldToSqlValue fieldDef) values)

{- |
  Migration Guide: @whereLike@ has renamed to @fieldLike@. It now takes a
  @T.Text@ value rather than a @String@.
-}
whereLike :: FieldDefinition nullability a -> String -> WhereCondition
whereLike fieldDef raw =
  WhereConditionExpr . expr $
  E.whereLike (fieldToNameForm fieldDef) (toSql raw)

{- |
  Migration Guide: @whereLikeInsensitive@ has renamed to
  @fieldLikeInsensitive@. It now takes a @T.Text@ value rather than a @String@.
-}
whereLikeInsensitive :: FieldDefinition nullability a -> String -> WhereCondition
whereLikeInsensitive fieldDef raw =
  WhereConditionExpr . expr $
  E.whereLikeInsensitive (fieldToNameForm fieldDef) (toSql raw)

{- |
  Migration Guide: @whereNotIn@ has renamed to @fieldNotIn@. It now takes a
  @NonEmpty@ list of values to reflect this is a requirement in SQL.
-}
whereNotIn :: FieldDefinition nullability a -> [a] -> WhereCondition
whereNotIn fieldDef values =
  WhereConditionExpr . expr $
  E.whereNotIn
    (fieldToNameForm fieldDef)
    (map (fieldToSqlValue fieldDef) values)

{- |
  Migration Guide: @whereQualified@ has been removed. If you need qualified
  column references you can use the SQL building functions found in
  @Orville.PostgreSQL.Expr@ to build them. The @qualifyColumn@ function can be
  use to qualified column references in that context. @BooleanExpr@ values
  built directly this way can be easily used in conjuction with other helpers
  such as @fieldEquals@ which also build @BooleanExpr@ values themselves.
-}
whereQualified :: TableDefinition a b c -> WhereCondition -> WhereCondition
whereQualified tableDef cond = Qualified tableDef cond

{- |
  Migration Guide: @whereRaw@ has been removed. In its place you should use the
  more general functions such as @unsafeSqlExpression@ or @unsafeRawSql@ in the
  @Orville.PostgreSQL.Raw.RawSql@ module to build a @BooleanExpr@.
-}
whereRaw :: String -> [SqlValue] -> WhereCondition
whereRaw str values = WhereConditionExpr . expr $ E.whereRaw str values

{- |
  Migration Guide: @isNull@ has been renamed to @fieldIsNull@
-}
isNull :: FieldDefinition Nullable a -> WhereCondition
isNull = WhereConditionExpr . expr . E.whereNull . fieldToNameForm

{- |
  Migration Guide: @isNotNull@ has been renamed to @fieldIsNotNull@
-}
isNotNull :: FieldDefinition Nullable a -> WhereCondition
isNotNull = WhereConditionExpr . expr . E.whereNotNull . fieldToNameForm

whereClause :: [WhereCondition] -> String
whereClause [] = ""
whereClause conds = "WHERE " ++ whereConditionSql (whereAnd conds)

whereValues :: [WhereCondition] -> [SqlValue]
whereValues = List.concatMap whereConditionValues

{- |
  Migration Guide: @whereToSql@ has been removed. It is replaced by the more
  generate @toBytesAndParams@ function in @Orville.PostgreSQL.Raw.RawSql@.
-}
whereToSql :: [WhereCondition] -> (String, [SqlValue])
whereToSql conds = (whereClause conds, whereValues conds)
