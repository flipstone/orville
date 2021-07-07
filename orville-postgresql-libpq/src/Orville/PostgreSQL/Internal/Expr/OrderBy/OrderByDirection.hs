{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByDirection
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection
  ( OrderByDirection,
    ascendingOrder,
    descendingOrder,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByDirection = OrderByDirection RawSql.RawSql
  deriving RawSql.SqlExpression

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"
