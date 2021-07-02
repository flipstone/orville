{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.Orville.PostgreSQL.Internal.Expr.LimitExpr
  ( LimitExpr,
    limitExpr,
  )
where

import qualified Database.Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype LimitExpr
  = LimitExpr RawSql.RawSql
  deriving (RawSql.ToRawSql)

limitExpr :: Int -> LimitExpr
limitExpr limitValue =
  LimitExpr $
    RawSql.fromString "LIMIT " <> RawSql.parameter (SqlValue.fromInt limitValue)
