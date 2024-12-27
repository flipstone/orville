{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

This module provides access to functions and types intended to help build SQL
statements directly in a relatively safe manner while also always providing a
way to write exactly the SQL you need. The SQL construction functions all
require specific types as their arguments to communicate what particular
fragment of SQL they expect to take as an argument. These types generally
provide a 'Orville.PostgreSQL.Raw.RawSql.SqlExpression' instance, however,
meaning that if Orville does not support constructing the exact SQL fragment
you want to pass for that argument directly you can always use
'Orville.PostgreSQL.Raw.RawSql.unsafeSqlExpression' to construct a value of the
required type. This means you can use as many of the "safe" construction
functions provided here as possible while substituting in hand-written SQL at
only the exact points you need.

For instance, the following code snippet shows how you could construct a @BEGIN
TRANSACTION ISOLATION LEVEL SOME NEW ISOLATION LEVEL@ statement if PostgreSQL
added @SOME NEW ISOLATION LEVEL@ and Orville did not yet support it.

@
  import qualified Orville.PostgreSQL.Expr as Expr
  import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

  customerBeginTransaction :: Expr.BeginTransactionExpr
  customerBeginTransaction =
    Expr.beginTransaction
      (Just (Expr.isolationLevel (RawSql.unsafeSqlExpression "SOME NEW ISOLATION LEVEL")))
@

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr
  ( module Orville.PostgreSQL.Expr.BinaryOperator
  , module Orville.PostgreSQL.Expr.ColumnDefinition
  , module Orville.PostgreSQL.Expr.Comment
  , module Orville.PostgreSQL.Expr.Count
  , module Orville.PostgreSQL.Expr.Cursor
  , module Orville.PostgreSQL.Expr.DataType
  , module Orville.PostgreSQL.Expr.Delete
  , module Orville.PostgreSQL.Expr.GroupBy
  , module Orville.PostgreSQL.Expr.FromItemExpr
  , module Orville.PostgreSQL.Expr.IfExists
  , module Orville.PostgreSQL.Expr.IfNotExists
  , module Orville.PostgreSQL.Expr.Index
  , module Orville.PostgreSQL.Expr.Insert
  , module Orville.PostgreSQL.Expr.OnConflict
  , module Orville.PostgreSQL.Expr.Join
  , module Orville.PostgreSQL.Expr.LimitExpr
  , module Orville.PostgreSQL.Expr.Math
  , module Orville.PostgreSQL.Expr.Name
  , module Orville.PostgreSQL.Expr.OffsetExpr
  , module Orville.PostgreSQL.Expr.OrderBy
  , module Orville.PostgreSQL.Expr.Query
  , module Orville.PostgreSQL.Expr.ReturningExpr
  , module Orville.PostgreSQL.Expr.Select
  , module Orville.PostgreSQL.Expr.SequenceDefinition
  , module Orville.PostgreSQL.Expr.TableConstraint
  , module Orville.PostgreSQL.Expr.TableDefinition
  , module Orville.PostgreSQL.Expr.Time
  , module Orville.PostgreSQL.Expr.Transaction
  , module Orville.PostgreSQL.Expr.Update
  , module Orville.PostgreSQL.Expr.ValueExpression
  , module Orville.PostgreSQL.Expr.WhereClause
  , module Orville.PostgreSQL.Expr.Trigger
  , module Orville.PostgreSQL.Expr.Function
  , module Orville.PostgreSQL.Expr.OrReplace
  , module Orville.PostgreSQL.Expr.ConditionalExpr
  , module Orville.PostgreSQL.Expr.Window
  , module Orville.PostgreSQL.Expr.Vacuum
  , module Orville.PostgreSQL.Expr.Extension
  , module Orville.PostgreSQL.Expr.RowLocking
  , module Orville.PostgreSQL.Expr.Aggregate
  , module Orville.PostgreSQL.Expr.Filter
  , module Orville.PostgreSQL.Expr.FetchClause
  , module Orville.PostgreSQL.Expr.Values
  , module Orville.PostgreSQL.Expr.TextSearch
  )
where

-- Note: we list the re-exports explicity above to control the order that they
-- appear in the generated haddock documentation.

import Orville.PostgreSQL.Expr.Aggregate
import Orville.PostgreSQL.Expr.BinaryOperator
import Orville.PostgreSQL.Expr.ColumnDefinition
import Orville.PostgreSQL.Expr.Comment
import Orville.PostgreSQL.Expr.ConditionalExpr
import Orville.PostgreSQL.Expr.Count
import Orville.PostgreSQL.Expr.Cursor
import Orville.PostgreSQL.Expr.DataType
import Orville.PostgreSQL.Expr.Delete
import Orville.PostgreSQL.Expr.Extension
import Orville.PostgreSQL.Expr.FetchClause
import Orville.PostgreSQL.Expr.Filter
import Orville.PostgreSQL.Expr.FromItemExpr
import Orville.PostgreSQL.Expr.Function
import Orville.PostgreSQL.Expr.GroupBy
import Orville.PostgreSQL.Expr.IfExists
import Orville.PostgreSQL.Expr.IfNotExists
import Orville.PostgreSQL.Expr.Index
import Orville.PostgreSQL.Expr.Insert
import Orville.PostgreSQL.Expr.Join
import Orville.PostgreSQL.Expr.LimitExpr
import Orville.PostgreSQL.Expr.Math
import Orville.PostgreSQL.Expr.Name
import Orville.PostgreSQL.Expr.OffsetExpr
import Orville.PostgreSQL.Expr.OnConflict
import Orville.PostgreSQL.Expr.OrReplace
import Orville.PostgreSQL.Expr.OrderBy
import Orville.PostgreSQL.Expr.Query
import Orville.PostgreSQL.Expr.ReturningExpr
import Orville.PostgreSQL.Expr.RowLocking
import Orville.PostgreSQL.Expr.Select
import Orville.PostgreSQL.Expr.SequenceDefinition
import Orville.PostgreSQL.Expr.TableConstraint
import Orville.PostgreSQL.Expr.TableDefinition
import Orville.PostgreSQL.Expr.TextSearch
import Orville.PostgreSQL.Expr.Time
import Orville.PostgreSQL.Expr.Transaction
import Orville.PostgreSQL.Expr.Trigger
import Orville.PostgreSQL.Expr.Update
import Orville.PostgreSQL.Expr.Vacuum
import Orville.PostgreSQL.Expr.ValueExpression
import Orville.PostgreSQL.Expr.Values
import Orville.PostgreSQL.Expr.WhereClause
import Orville.PostgreSQL.Expr.Window
