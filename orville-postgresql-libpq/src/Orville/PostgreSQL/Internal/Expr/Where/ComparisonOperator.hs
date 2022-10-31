{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Where.ComparisonOperator
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator
  ( ComparisonOperator,
    equalsOp,
    notEqualsOp,
    greaterThanOp,
    lessThanOp,
    greaterThanOrEqualsOp,
    lessThanOrEqualsOp,
    likeOp,
    iLikeOp,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ComparisonOperator
  = ComparisonOperator RawSql.RawSql
  deriving (RawSql.SqlExpression)

equalsOp :: ComparisonOperator
equalsOp =
  ComparisonOperator (RawSql.fromString "=")

notEqualsOp :: ComparisonOperator
notEqualsOp =
  ComparisonOperator (RawSql.fromString "<>")

greaterThanOp :: ComparisonOperator
greaterThanOp =
  ComparisonOperator (RawSql.fromString ">")

lessThanOp :: ComparisonOperator
lessThanOp =
  ComparisonOperator (RawSql.fromString "<")

greaterThanOrEqualsOp :: ComparisonOperator
greaterThanOrEqualsOp =
  ComparisonOperator (RawSql.fromString ">=")

lessThanOrEqualsOp :: ComparisonOperator
lessThanOrEqualsOp =
  ComparisonOperator (RawSql.fromString "<=")

likeOp :: ComparisonOperator
likeOp =
  ComparisonOperator (RawSql.fromString "LIKE")

iLikeOp :: ComparisonOperator
iLikeOp =
  ComparisonOperator (RawSql.fromString "ILIKE")
