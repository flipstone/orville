{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.LimitExpr
  ( LimitExpr,
    limitExpr,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype LimitExpr
  = LimitExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

limitExpr :: Int -> LimitExpr
limitExpr limitValue =
  LimitExpr $
    RawSql.fromString "LIMIT " <> RawSql.parameter (SqlValue.fromInt limitValue)
