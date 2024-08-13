{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Insert
  ( InsertExpr
  , insertExpr
  , InsertColumnList
  , insertColumnList
  , InsertSource
  , insertSqlValues
  , RowValues
  , rowValues
  , OnConflictExpr
  , onConflictDoNothing
  , onConflictDoUpdate
  , excludedColumnName
  , setExcludedColumn
  , setExcludedClauseList
  , conflictTargetColumnName
  , conflictTargetConstraintName
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Internal.Name.ConstraintName (ConstraintName)
import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Raw.SqlValue (SqlValue)

{- |
Type to represent a SQL "INSERT" statement. E.G.

> INSERT INTO foo (id) VALUES (1),(3),(3)

'InsertExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype InsertExpr
  = InsertExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create an 'InsertExpr' for the given 'TableName', limited to the specific columns if
given. Callers of this likely want to use a function to create the 'InsertSource' to ensure the
input values are correctly used as parameters. This function does not include that protection
itself.

@since 1.0.0.0
-}
insertExpr ::
  Qualified TableName ->
  Maybe InsertColumnList ->
  InsertSource ->
  Maybe OnConflictExpr ->
  Maybe ReturningExpr ->
  InsertExpr
insertExpr target maybeInsertColumns source maybeOnConflict maybeReturning =
  InsertExpr
    . RawSql.intercalate RawSql.space
    $ catMaybes
      [ Just $ RawSql.fromString "INSERT INTO"
      , Just $ RawSql.toRawSql target
      , fmap RawSql.toRawSql maybeInsertColumns
      , Just $ RawSql.toRawSql source
      , fmap RawSql.toRawSql maybeOnConflict
      , fmap RawSql.toRawSql maybeReturning
      ]

{- |
Type to represent the SQL columns list for an insert statement. E.G.

> (foo,bar,baz)

'InsertColumnList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype InsertColumnList
  = InsertColumnList RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create an 'InsertColumnList' for the given 'ColumnName's, making sure the columns are wrapped in
parens and commas are used to separate.

@since 1.0.0.0
-}
insertColumnList :: [Qualified ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    RawSql.leftParen
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql columnNames)
      <> RawSql.rightParen

{- |
Type to represent the SQL for the source of data for an insert statement. E.G.

> VALUES ('Bob',32),('Cindy',33)

'InsertSource' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype InsertSource
  = InsertSource RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create an 'InsertSource' for the given 'RowValues'. This ensures that all input values are used
as parameters and comma-separated in the generated SQL.

@since 1.0.0.0
-}
insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql rows)

{- | Create an 'InsertSource' for the given 'SqlValue's. This ensures that all input values are used
as parameters and comma-separated in the generated SQL.

@since 1.0.0.0
-}
insertSqlValues :: [[SqlValue]] -> InsertSource
insertSqlValues =
  insertRowValues . fmap rowValues

{- |
Type to represent a SQL row literal. For example, a single row to insert
in a @VALUES@ clause. E.G.

> ('Cindy',33)

'RowValues' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype RowValues
  = RowValues RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create a 'RowValues' for the given 'SqlValue's. This ensures that all input values are used as
parameters and comma-separated in the generated SQL.

@since 1.0.0.0
-}
rowValues :: [SqlValue] -> RowValues
rowValues values =
  RowValues $
    mconcat
      [ RawSql.leftParen
      , RawSql.intercalate RawSql.comma (fmap RawSql.parameter values)
      , RawSql.rightParen
      ]

{- |
Type to represent the SQL for the 'ON CONFLICT' clause.

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

{- | Create an 'OnConflict' that specifies no action is to be taken during a conflicting insert.

@since 1.1.0.0
-}
onConflictDoNothing :: OnConflictExpr
onConflictDoNothing =
  OnConflictExpr $ RawSql.fromString "ON CONFLICT DO NOTHING"

{- | Create `conflict target` that Specifies which conflicts ON CONFLICT takes
   the alternative action on by choosing arbiter indexes or names a constraint explicitly.

   Developer needs to take care whether provided column name is index or not.
-}
data ConflictTarget
  = CTColName (Qualified ColumnName)
  | CTConstraintName ConstraintName

{-
Constructs a 'ConflictTarget' using qualified column name. This ConflictTarget can be used for
On CONFLICT DO Update
-}
conflictTargetColumnName :: Qualified ColumnName -> ConflictTarget
conflictTargetColumnName = CTColName

{-
Constructs a 'ConflictTarget' using ConstraintName. This ConflictTarget can be used for
On CONFLICT DO Update
-}
conflictTargetConstraintName :: ConstraintName -> ConflictTarget
conflictTargetConstraintName = CTConstraintName

{- |
Type to represent the list of updates to be made in an `ON CONFLICT _ DO UPDATE` statement. E.G.

> foo = EXCLUDED.foo,
> bar = EXCLUDED.bar

'SetExcludedClauseList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.
-}
newtype SetExcludedClauseList
  = SetExcludedClauseList RawSql.RawSql
  deriving (RawSql.SqlExpression)

setExcludedClauseList :: NonEmpty SetExcludedClause -> SetExcludedClauseList
setExcludedClauseList =
  SetExcludedClauseList . RawSql.intercalate RawSql.comma

{- |
Type to represent a single update to be made in an `ON CONFLICT _ DO UPDATE` statement. E.G.

> foo = EXCLUDED.foo

'SetExcludedClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.
-}
newtype SetExcludedClause
  = SetExcludedClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SetExcludedClause' that will set the specified column to the specified
  EXCLUDED columns.
-}
setExcludedColumn ::
  Qualified ColumnName ->
  Excluded ColumnName ->
  SetExcludedClause
setExcludedColumn columnName excludedColumnName0 =
  SetExcludedClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.toRawSql excludedColumnName0

{- |
Type to represent an excluded SQL name. E.G.

> "EXCLUDED"."some_column_name"

'Excluded' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.
-}
newtype Excluded columnName = Excluded RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
Optionally excludes a 'ColumnName' with 'EXCLUDED'.
-}
excludedColumnName :: Qualified ColumnName -> Excluded ColumnName
excludedColumnName colName =
  Excluded $ RawSql.fromString "EXCLUDED" <> RawSql.dot <> RawSql.toRawSql colName

{- |
   Constructs an 'ON CONFLICT' with the given options.
-}
onConflictDoUpdate ::
  ConflictTarget ->
  SetExcludedClauseList ->
  Maybe WhereClause ->
  OnConflictExpr
onConflictDoUpdate conflictTarget excludedClauseList maybeWhereClause =
  OnConflictExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just (RawSql.fromString "ON CONFLICT")
          , Just conflictTargetToRawSql
          , Just (RawSql.fromString "DO UPDATE SET")
          , Just (RawSql.toRawSql excludedClauseList)
          , RawSql.toRawSql <$> maybeWhereClause
          ]
      )
 where
  conflictTargetToRawSql =
    case conflictTarget of
      CTColName colName ->
        RawSql.leftParen <> RawSql.toRawSql colName <> RawSql.rightParen
      CTConstraintName constraintName_ ->
        RawSql.fromString "ON CONSTRAINT " <> RawSql.toRawSql constraintName_
