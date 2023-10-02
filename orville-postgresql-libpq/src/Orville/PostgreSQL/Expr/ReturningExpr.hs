{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.ReturningExpr
  ( ReturningExpr
  , returningExpr
  )
where

import Orville.PostgreSQL.Expr.Query (SelectList)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a @RETURNING@ clause in a SQL @SELECT@ statement. E.G.

> RETURNING (id)

'ReturningExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype ReturningExpr
  = ReturningExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'ReturningExpr' that returns the items given in the
  'SelectList'. Essentialy this retults @RETURNING <SelectList items>@
-}
returningExpr :: SelectList -> ReturningExpr
returningExpr selectList =
  ReturningExpr $
    RawSql.fromString "RETURNING "
      <> RawSql.toRawSql selectList
