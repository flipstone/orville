{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Select.SelectClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Select.SelectClause
  ( SelectClause,
    selectClause,
  )
where

import qualified Orville.PostgreSQL.Internal.Expr.Select.SelectExpr as Expr
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SelectClause
  = SelectClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectClause :: Expr.SelectExpr -> SelectClause
selectClause expr = SelectClause (RawSql.fromString "SELECT " <> RawSql.toRawSql expr)
