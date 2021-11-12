{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.ReturningExpr
  ( ReturningExpr,
    returningExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.Query (SelectList)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ReturningExpr
  = ReturningExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

returningExpr :: SelectList -> ReturningExpr
returningExpr selectList =
  ReturningExpr $
    RawSql.fromString "RETURNING "
      <> RawSql.toRawSql selectList
