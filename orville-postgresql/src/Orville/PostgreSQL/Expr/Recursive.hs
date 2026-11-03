{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.Recursive
  ( Recursive
  , recursive
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the @RECURSIVE@ keyword, which lets a query refer to the
  view or common table expression it defines. E.G.

> RECURSIVE

'Recursive' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype Recursive
  = Recursive RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @RECURSIVE@ keyword.

@since 1.2.0.0
-}
recursive :: Recursive
recursive =
  Recursive (RawSql.fromString "RECURSIVE")
