{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.LimitExpr
  ( LimitExpr
  , limitExpr
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | Type to represent a SQL limit expression. E.G.

> LIMIT 10

'LimitExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype LimitExpr
  = LimitExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create a 'LimitExpr' for the given 'Int'. This ensures that the input value is used
as a parameter in the generated SQL.

@since 1.0.0.0
-}
limitExpr :: Int -> LimitExpr
limitExpr limitValue =
  LimitExpr $
    RawSql.fromString "LIMIT "
      <> RawSql.parameter (SqlValue.fromInt limitValue)
