{-|
Module    : Database.Orville.PostgreSQL.Expr.Where.ComparisonOperator
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Where.ComparisonOperator
  ( ComparisonOperator
  , comparisonOperatorToSql
  , equalsOp
  , notEqualsOp
  , greaterThanOp
  , lessThanOp
  , greaterThanOrEqualsOp
  , lessThanOrEqualsOp
  ) where

import           Database.Orville.PostgreSQL.Internal.RawSql (RawSql, fromString)

newtype ComparisonOperator =
  ComparisonOperator RawSql

comparisonOperatorToSql :: ComparisonOperator -> RawSql
comparisonOperatorToSql (ComparisonOperator sql) = sql

equalsOp :: ComparisonOperator
equalsOp =
  ComparisonOperator (fromString "=")

notEqualsOp :: ComparisonOperator
notEqualsOp =
  ComparisonOperator (fromString "<>")

greaterThanOp :: ComparisonOperator
greaterThanOp =
  ComparisonOperator (fromString ">")

lessThanOp :: ComparisonOperator
lessThanOp =
  ComparisonOperator (fromString "<")

greaterThanOrEqualsOp :: ComparisonOperator
greaterThanOrEqualsOp =
  ComparisonOperator (fromString ">=")

lessThanOrEqualsOp :: ComparisonOperator
lessThanOrEqualsOp =
  ComparisonOperator (fromString "<=")
