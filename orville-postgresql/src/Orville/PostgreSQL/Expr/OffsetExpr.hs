{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.OffsetExpr
  ( OffsetExpr
  , offsetExpr
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | Type to represent a SQL offset expression. E.G.

> OFFSET 10

'OffsetExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype OffsetExpr
  = OffsetExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- | Create an 'OffsetExpr' for the given 'Int'. This ensures that the input value is used
as a parameter in the generated SQL.

@since 1.0.0.0
-}
offsetExpr :: Int -> OffsetExpr
offsetExpr offsetValue =
  OffsetExpr $
    RawSql.fromString "OFFSET " <> RawSql.parameter (SqlValue.fromInt offsetValue)
