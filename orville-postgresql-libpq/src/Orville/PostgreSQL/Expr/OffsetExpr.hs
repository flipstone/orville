{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.OffsetExpr
  ( OffsetExpr,
    offsetExpr,
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

newtype OffsetExpr
  = OffsetExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

offsetExpr :: Int -> OffsetExpr
offsetExpr offsetValue =
  OffsetExpr $
    RawSql.fromString "OFFSET " <> RawSql.parameter (SqlValue.fromInt offsetValue)
