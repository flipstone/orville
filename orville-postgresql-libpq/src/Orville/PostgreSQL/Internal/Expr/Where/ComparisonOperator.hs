{- |
Module    : Orville.PostgreSQL.Expr.Where.ComparisonOperator
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator
  ( ComparisonOperator,
    comparisonOperatorToSql,
    equalsOp,
    notEqualsOp,
    greaterThanOp,
    lessThanOp,
    greaterThanOrEqualsOp,
    lessThanOrEqualsOp,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ComparisonOperator
  = ComparisonOperator RawSql.RawSql

comparisonOperatorToSql :: ComparisonOperator -> RawSql.RawSql
comparisonOperatorToSql (ComparisonOperator sql) = sql

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
