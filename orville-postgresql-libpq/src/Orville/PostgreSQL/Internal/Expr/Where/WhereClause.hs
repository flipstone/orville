{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.WhereClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.WhereClause
  ( WhereClause,
    whereClause,
  )
where

import Orville.PostgreSQL.Internal.Expr.Where.BooleanExpr (BooleanExpr)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype WhereClause
  = WhereClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    RawSql.fromString "WHERE " <> RawSql.toRawSql booleanExpr
