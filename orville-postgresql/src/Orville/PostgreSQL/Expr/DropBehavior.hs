{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.DropBehavior
  ( DropBehavior
  , cascade
  , restrict
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the drop behavior at the end of a @DROP@ statement,
  which decides what happens to objects that depend on the one being dropped.
  E.G.

> CASCADE

'DropBehavior' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype DropBehavior
  = DropBehavior RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @CASCADE@ drop behavior, which also drops the objects that depend on
  the one being dropped.

@since 1.2.0.0
-}
cascade :: DropBehavior
cascade =
  DropBehavior (RawSql.fromString "CASCADE")

{- | The @RESTRICT@ drop behavior, which refuses to drop an object that other
  objects depend on. This is the behavior when no drop behavior is given.

@since 1.2.0.0
-}
restrict :: DropBehavior
restrict =
  DropBehavior (RawSql.fromString "RESTRICT")
