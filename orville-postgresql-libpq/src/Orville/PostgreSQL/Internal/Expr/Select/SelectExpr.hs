{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Select.SelectExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Select.SelectExpr
  ( SelectExpr,
    selectExpr,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SelectExpr = SelectExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectExpr :: Bool -> SelectExpr
selectExpr isDistinct =
  SelectExpr . RawSql.fromString $ if isDistinct then "DISTINCT " else ""
