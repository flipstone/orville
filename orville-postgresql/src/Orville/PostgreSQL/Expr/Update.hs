{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Update
  ( UpdateExpr
  , updateExpr
  , SetClauseList
  , setClauseList
  , SetClause
  , setColumn
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
Type to represent a SQL @UPDATE@ statement. E.G.

> UPDATE foo
> SET id = 1
> WHERE id <> 1

'UpdateExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype UpdateExpr
  = UpdateExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs an 'UpdateExpr' with the given options.

  @since 1.0.0.0
-}
updateExpr ::
  -- | The name of the table to be updated.
  Qualified TableName ->
  -- | The updates to be made to the table.
  SetClauseList ->
  -- | An optional where clause to limit the rows updated.
  Maybe WhereClause ->
  -- | An optional returning clause to return data from the updated rows.
  Maybe ReturningExpr ->
  UpdateExpr
updateExpr tableName setClause maybeWhereClause maybeReturningExpr =
  UpdateExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "UPDATE"
        , Just $ RawSql.toRawSql tableName
        , Just $ RawSql.fromString "SET"
        , Just $ RawSql.toRawSql setClause
        , fmap RawSql.toRawSql maybeWhereClause
        , fmap RawSql.toRawSql maybeReturningExpr
        ]

{- |
Type to represent the list of updates to be made in an @UPDATE@ statement. E.G.

> foo = 1,
> bar = 2

'SetClauseList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SetClauseList
  = SetClauseList RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SetClauseList' with the specified set clauses.

  @since 1.0.0.0
-}
setClauseList :: NonEmpty SetClause -> SetClauseList
setClauseList =
  SetClauseList . RawSql.intercalate RawSql.comma

{- |
Type to represent a single update to be made in an @UPDATE@ statement. E.G.

> foo = 1

'SetClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SetClause
  = SetClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SetClause' that will set the specified column to the specified
  value.

  @since 1.0.0.0
-}
setColumn :: Qualified ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value
