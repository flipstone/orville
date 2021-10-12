{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.ReturningExpr
  ( ReturningExpr,
    returningExpr,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ReturningExpr
  = ReturningExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

returningExpr :: [ColumnName] -> ReturningExpr
returningExpr columns =
  ReturningExpr $
    RawSql.fromString "RETURNING "
      <> RawSql.intercalate RawSql.comma columns
