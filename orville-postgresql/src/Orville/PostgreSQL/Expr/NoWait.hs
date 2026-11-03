{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.NoWait
  ( NoWait
  , noWait
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the @NOWAIT@ keyword at the end of a statement that
  would otherwise wait for a lock, making it fail instead. E.G.

> NOWAIT

'NoWait' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype NoWait
  = NoWait RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @NOWAIT@ keyword.

@since 1.2.0.0
-}
noWait :: NoWait
noWait =
  NoWait (RawSql.fromString "NOWAIT")
