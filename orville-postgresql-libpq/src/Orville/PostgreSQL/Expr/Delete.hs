{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Provides a type representing SQL DELETE and construction of that type.

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.Delete
  ( DeleteExpr
  , deleteExpr
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (Qualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL delete statement.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a delete statement by hand and use it in a
place that expected a 'DeleteExpr', that could be done as

 > RawSql.unsafeSqlExpression "DELETE FROM <my things to delete>"

@since 0.10.0.0
-}
newtype DeleteExpr
  = DeleteExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- |

Construct a SQL DELETE from a table, optionally limiting with a 'WhereClause' and optionally
returning a 'ReturningExpr'.

@since 0.10.0.0
-}
deleteExpr ::
  Qualified TableName ->
  Maybe WhereClause ->
  Maybe ReturningExpr ->
  DeleteExpr
deleteExpr tableName maybeWhereClause maybeReturningExpr =
  DeleteExpr $
    RawSql.intercalate RawSql.space $
      catMaybes
        [ Just $ RawSql.fromString "DELETE FROM"
        , Just $ RawSql.toRawSql tableName
        , fmap RawSql.toRawSql maybeWhereClause
        , fmap RawSql.toRawSql maybeReturningExpr
        ]
