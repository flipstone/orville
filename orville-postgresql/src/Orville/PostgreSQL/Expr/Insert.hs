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
  , valuesExprInsertSource
  )
where

import Data.Maybe (catMaybes)

import qualified Data.List.NonEmpty as NE
import Orville.PostgreSQL.Expr.Name (ColumnName, QualifiedOrUnqualified, TableName)
import Orville.PostgreSQL.Expr.OnConflict (OnConflictExpr)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.Values (ValuesExpr)
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
  QualifiedOrUnqualified TableName ->
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
insertColumnList :: [QualifiedOrUnqualified ColumnName] -> InsertColumnList
insertColumnList columnNames =
  InsertColumnList $
    maybe
      mempty
      (RawSql.parenthesized . RawSql.intercalate RawSql.comma . fmap RawSql.toRawSql)
      (NE.nonEmpty columnNames)

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

{- |
  Use a 'ValuesExpr' as an 'InsertSource'.

@since 1.1.0.0
-}
valuesExprInsertSource :: ValuesExpr -> InsertSource
valuesExprInsertSource = InsertSource . RawSql.toRawSql

{-# DEPRECATED insertRowValues "Use Orville.PostgreSQL.Expr.valuesExpr and Orville.PostgreSQL.Expr.valuesExprInsertSource" #-}

{- | Create an 'InsertSource' for the given 'RowValues'. This ensures that all input values are used
as parameters and comma-separated in the generated SQL.

@since 1.0.0.0
-}
insertRowValues :: [RowValues] -> InsertSource
insertRowValues rows =
  InsertSource $
    RawSql.fromString "VALUES "
      <> RawSql.intercalate RawSql.comma (fmap RawSql.toRawSql rows)

{-# DEPRECATED insertSqlValues "Use Orville.PostgreSQL.Expr.ValuesExpr and Orivlle.PostgreSQL.Expr.valuesExprInsertSource" #-}

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
