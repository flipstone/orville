{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.LimitExpr
  ( LimitExpr,
    limitExpr,
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

newtype LimitExpr
  = LimitExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

limitExpr :: Int -> LimitExpr
limitExpr limitValue =
  LimitExpr $
    RawSql.fromString "LIMIT "
      <> RawSql.parameter (SqlValue.fromInt limitValue)
