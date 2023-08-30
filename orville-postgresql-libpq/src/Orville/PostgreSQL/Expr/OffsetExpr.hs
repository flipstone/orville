{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Expr.OffsetExpr
  ( OffsetExpr
  , offsetExpr
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
Type to represent a SQL offset expression (e.g. @OFFSET ...@)

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a offset expression by hand and
use it in a place that expected a 'OffsetExpr', that could be done as

 > RawSql.unsafeSqlExpression "OFFSET some expression"

@since 0.10.0.0
-}
newtype OffsetExpr
  = OffsetExpr RawSql.RawSql
  deriving
    ( -- | @since 0.10.0.0
      RawSql.SqlExpression
    )

{- | Create an 'OffsetExpr' for the given 'Int'. This ensures that the input value is used
as parameters in the generated SQL.

@since 0.10.0.0
-}
offsetExpr :: Int -> OffsetExpr
offsetExpr offsetValue =
  OffsetExpr $
    RawSql.fromString "OFFSET " <> RawSql.parameter (SqlValue.fromInt offsetValue)
