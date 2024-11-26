{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.OnConflict
  ( OnConflictExpr
  , onConflict
  , onConflictDoNothing
  , onConflictDoUpdate
  , ConflictTargetExpr
  , conflictTargetForIndexColumn
  , conflictTargetForIndexExpr
  , conflictTargetForConstraint
  , ConflictActionExpr
  , ConflictSetItemExpr
  , setColumnNameExcluded
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Expr.Index (IndexBodyExpr)
import Orville.PostgreSQL.Expr.Internal.Name.ConstraintName (ConstraintName)
import Orville.PostgreSQL.Expr.Name (ColumnName, QualifiedOrUnqualified)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the SQL for the 'ON CONFLICT' clause.

'OnConflict' provides a 'RawSql.SqlExpression' instance. See 'RawSql.unsafeSqlExpression' for how to
construct a value with your own custom SQL.

@since 1.1.0.0
-}
newtype OnConflictExpr
  = OnConflictExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Create an 'OnConflict' that performs the specified action to be taken during a conflicting insert.

@since 1.1.0.0
-}
onConflict :: Maybe ConflictTargetExpr -> ConflictActionExpr -> OnConflictExpr
onConflict mbTarget action =
  OnConflictExpr $
    RawSql.fromString "ON CONFLICT "
      <> case mbTarget of
        Nothing ->
          RawSql.toRawSql action
        Just target ->
          RawSql.toRawSql target <> RawSql.space <> RawSql.toRawSql action

{- | A wrapper around 'onConflict' for no specific target and an explicit no action to perform.

@since 1.1.0.0
-}
onConflictDoNothing :: OnConflictExpr
onConflictDoNothing =
  onConflict Nothing doNothingActionExpr

{- | A wrapper around 'onConflict' to perform an update, with optional target and where clause. That
   will use the given 'ConflictSetItemExpr's as the update body.

@since 1.1.0.0
-}
onConflictDoUpdate :: Maybe ConflictTargetExpr -> NonEmpty ConflictSetItemExpr -> Maybe WhereClause -> OnConflictExpr
onConflictDoUpdate mbTarget setItems =
  onConflict mbTarget . doUpdateSetActionExpr setItems

{- | Type to represent the target portion of the SQL 'ON CONFLICT target action'

@since 1.1.0.0
-}
newtype ConflictTargetExpr
  = ConflictTargetExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Build a 'ConflictTargetExpr' that would conflict on a single 'ColumnName'. PostgreSQL will use
   the column to infer which index to check for conflicts.

@since 1.1.0.0
-}
conflictTargetForIndexColumn :: QualifiedOrUnqualified ColumnName -> Maybe WhereClause -> ConflictTargetExpr
conflictTargetForIndexColumn colName mbWhere =
  let
    parensCol = RawSql.parenthesized $ RawSql.toRawSql colName
  in
    ConflictTargetExpr $
      case mbWhere of
        Nothing -> parensCol
        Just whereClause -> parensCol <> RawSql.space <> RawSql.toRawSql whereClause

{- | Build a 'ConflictTargetExpr' that would conflict on an 'IndexBodyExpr'. This can be used to check
   for more than a single column. PostgreSQL will use the expression to infer which index to check for
   conflicts.

@since 1.1.0.0
-}
conflictTargetForIndexExpr :: IndexBodyExpr -> Maybe WhereClause -> ConflictTargetExpr
conflictTargetForIndexExpr idxBody mbWhere =
  let
    rawIdxBody = RawSql.toRawSql idxBody
  in
    ConflictTargetExpr $
      case mbWhere of
        Nothing -> rawIdxBody
        Just whereClause -> rawIdxBody <> RawSql.space <> RawSql.toRawSql whereClause

{- | Build a 'ConflictTargetExpr' for conflicting on the given 'ConstraintName'. For conflicing on an
   index, see 'conflictTargetForIndexColumn' or 'conflictTargetForIndexExpr' instead.

@since 1.1.0.0
-}
conflictTargetForConstraint :: ConstraintName -> ConflictTargetExpr
conflictTargetForConstraint constraintName =
  ConflictTargetExpr $ RawSql.fromString "ON CONSTRAINT " <> RawSql.toRawSql constraintName

{- | Type to represent the action portion of the SQL 'ON CONFLICT target action'

@since 1.1.0.0
-}
newtype ConflictActionExpr
  = ConflictActionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | A 'ConflictActionExpr' for the SQL 'DO NOTHING'.

@since 1.1.0.0
-}
doNothingActionExpr :: ConflictActionExpr
doNothingActionExpr =
  ConflictActionExpr $ RawSql.fromString "DO NOTHING"

{- | A 'ConflictActionExpr' for the SQL 'DO UPDATE SET', that will use the given 'ConflictSetItemExpr'
   to determine which columns to update and the optional 'WhereClause' to peform filtering.

@since 1.1.0.0
-}
doUpdateSetActionExpr :: NonEmpty ConflictSetItemExpr -> Maybe WhereClause -> ConflictActionExpr
doUpdateSetActionExpr setItems mbWhere =
  let
    commaSeparatedItems = RawSql.intercalate RawSql.comma setItems
  in
    ConflictActionExpr $
      RawSql.fromString "DO UPDATE SET "
        <> case mbWhere of
          Nothing -> commaSeparatedItems
          Just whereClause -> commaSeparatedItems <> RawSql.space <> RawSql.toRawSql whereClause

{- | Type to represent the item to be set in an 'ON CONFLICT DO UPDATE'. E.G.

> ON CONFLICT DO UPDATE SET item

@since 1.1.0.0
-}
newtype ConflictSetItemExpr
  = ConflictSetItemExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Builds a 'ConflictSetItemExpr' that will set the given 'ColumnName' to the excluded value using
the special pseudo-table 'EXCLUDED'.

@since 1.1.0.0
-}
setColumnNameExcluded :: QualifiedOrUnqualified ColumnName -> ConflictSetItemExpr
setColumnNameExcluded colName =
  let
    rawName = RawSql.toRawSql colName
  in
    ConflictSetItemExpr $
      rawName
        <> RawSql.fromString " = "
        <> RawSql.fromString "EXCLUDED"
        <> RawSql.dot
        <> rawName
