{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Expr.Temporary
  ( Temporary
  , temporary
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Type to represent the @TEMPORARY@ keyword of a @CREATE@ statement, which
  makes the created object exist only for the current session. E.G.

> TEMPORARY

'Temporary' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.2.0.0
-}
newtype Temporary
  = Temporary RawSql.RawSql
  deriving
    ( -- | @since 1.2.0.0
      RawSql.SqlExpression
    )

{- | The @TEMPORARY@ keyword.

@since 1.2.0.0
-}
temporary :: Temporary
temporary =
  Temporary (RawSql.fromString "TEMPORARY")
