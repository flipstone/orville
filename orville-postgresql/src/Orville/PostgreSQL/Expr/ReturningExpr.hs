{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.ReturningExpr
  ( ReturningExpr
  , returningExpr
  )
where

import Orville.PostgreSQL.Expr.Query (SelectList)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent a @RETURNING@ clause in a SQL @SELECT@ statement. E.G.

> RETURNING (id)

'ReturningExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype ReturningExpr
  = ReturningExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'ReturningExpr' that returns the items given in the
  'SelectList'. Essentialy this retults @RETURNING <SelectList items>@.

@since 1.0.0.0
-}
returningExpr :: SelectList -> ReturningExpr
returningExpr selectList =
  ReturningExpr $
    RawSql.fromString "RETURNING "
      <> RawSql.toRawSql selectList
