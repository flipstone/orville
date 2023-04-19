{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.ReturningExpr
  ( ReturningExpr,
    returningExpr,
  )
where

import Orville.PostgreSQL.Expr.Query (SelectList)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype ReturningExpr
  = ReturningExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

returningExpr :: SelectList -> ReturningExpr
returningExpr selectList =
  ReturningExpr $
    RawSql.fromString "RETURNING "
      <> RawSql.toRawSql selectList
