{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

Provides a type representing SQL DELETE and construction of that type.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Delete
  ( DeleteExpr
  , deleteExpr
  )
where

import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.Name (QualifiedOrUnqualified, TableName)
import Orville.PostgreSQL.Expr.ReturningExpr (ReturningExpr)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL delete statement. E.G.

> DELETE FROM foo WHERE id < 10

'DeleteExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DeleteExpr
  = DeleteExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |

Construct a SQL DELETE from a table, optionally limiting with a 'WhereClause' and optionally
returning a 'ReturningExpr'.

@since 1.0.0.0
-}
deleteExpr ::
  QualifiedOrUnqualified TableName ->
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
