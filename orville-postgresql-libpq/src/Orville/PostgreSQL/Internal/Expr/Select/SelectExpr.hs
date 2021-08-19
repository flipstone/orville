{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Select.SelectExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Select.SelectExpr
  ( SelectExpr,
    selectExpr,
    Distinct (..),
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SelectExpr = SelectExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

data Distinct = Distinct

selectExpr :: Maybe Distinct -> SelectExpr
selectExpr mbDistinct =
  SelectExpr . RawSql.fromString $
    case mbDistinct of
      Just Distinct -> "DISTINCT "
      Nothing -> ""
