{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Select
  ( SelectClause
  , selectClause
  , SelectExpr
  , selectExpr
  , Distinct (Distinct)
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype SelectClause
  = SelectClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectClause :: SelectExpr -> SelectClause
selectClause expr = SelectClause (RawSql.fromString "SELECT " <> RawSql.toRawSql expr)

newtype SelectExpr = SelectExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

data Distinct = Distinct

selectExpr :: Maybe Distinct -> SelectExpr
selectExpr mbDistinct =
  SelectExpr . RawSql.fromString $
    case mbDistinct of
      Just Distinct -> "DISTINCT "
      Nothing -> ""
