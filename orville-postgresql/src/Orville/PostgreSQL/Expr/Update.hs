{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2025
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
  , setColumnExpression
  , UpdateNamedOnly
  , onlyUpdateNamedTable
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (ColumnName, QualifiedOrUnqualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.TableReferenceList (TableReferenceList)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | Type to represent a SQL @UPDATE@ statement. E.G.

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
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Constructs an 'UpdateExpr' with the given options.

@since 1.0.0.0
-}
updateExpr ::
  -- | The name of the table to be updated.
  QualifiedOrUnqualified TableName ->
  -- | Optionally update only the given table name
  Maybe UpdateNamedOnly ->
  -- | The updates to be made to the table.
  SetClauseList ->
  -- | An optional 'TableReferenceList' to allow columns from other tables to be included in the
  -- 'SetClauseList' and 'WhereClause'
  --
  -- @since 1.1.0.0
  Maybe TableReferenceList ->
  -- | An optional where clause to limit the rows updated.
  Maybe WhereClause ->
  -- | An optional returning clause to return data from the updated rows.
  Maybe ReturningExpr ->
  UpdateExpr
updateExpr tableName maybeUpdateNamedOnly setClause maybeTableRefs maybeWhereClause maybeReturningExpr =
  let
    buildTableRefs :: TableReferenceList -> RawSql.RawSql
    buildTableRefs tableRefs =
      RawSql.fromString "FROM " <> RawSql.toRawSql tableRefs
  in
    UpdateExpr
      . RawSql.intercalate RawSql.space
      $ catMaybes
        [ Just $ RawSql.fromString "UPDATE"
        , fmap RawSql.toRawSql maybeUpdateNamedOnly
        , Just $ RawSql.toRawSql tableName
        , Just $ RawSql.fromString "SET"
        , Just $ RawSql.toRawSql setClause
        , fmap buildTableRefs maybeTableRefs
        , fmap RawSql.toRawSql maybeWhereClause
        , fmap RawSql.toRawSql maybeReturningExpr
        ]

{- | Type to represent the option to update only the named table and not any descendants. E.G.

> ONLY

'UpdateNamedOnly' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype UpdateNamedOnly
  = UpdateNamedOnly RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @ONLY@ modifier to an @UPDATE@ to not update any descendant tables.

@since 1.1.0.0
-}
onlyUpdateNamedTable :: UpdateNamedOnly
onlyUpdateNamedTable =
  UpdateNamedOnly $ RawSql.fromString "ONLY"

{- | Type to represent the list of updates to be made in an @UPDATE@ statement. E.G.

> foo = 1,
> bar = 2

'SetClauseList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SetClauseList
  = SetClauseList RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'SetClauseList' with the specified set clauses.

  @since 1.0.0.0
-}
setClauseList :: NonEmpty SetClause -> SetClauseList
setClauseList =
  SetClauseList . RawSql.intercalate RawSql.comma

{- | Type to represent a single update to be made in an @UPDATE@ statement. E.G.

> foo = 1

'SetClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SetClause
  = SetClause RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'SetClause' that will set the specified column to the specified
  value.

  @since 1.0.0.0
-}
setColumn :: QualifiedOrUnqualified ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value

{- | Constructs a 'SetClause' that will set the specified columns to the expression.

@since 1.1.0.0
-}
setColumnExpression ::
  NonEmpty (QualifiedOrUnqualified ColumnName) ->
  ValueExpression ->
  SetClause
setColumnExpression columnNames value =
  SetClause $
    RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)
      <> RawSql.fromString "="
      <> RawSql.parenthesized (RawSql.toRawSql value)
