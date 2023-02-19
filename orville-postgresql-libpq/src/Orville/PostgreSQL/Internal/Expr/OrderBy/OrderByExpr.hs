{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.OrderBy.OrderByExpr
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByExpr
  ( OrderByExpr,
    appendOrderByExpr,
    orderByColumnName,
    orderByExpr,
    orderByColumnsExpr,
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import Orville.PostgreSQL.Internal.Expr.OrderBy.OrderByDirection (OrderByDirection)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype OrderByExpr = OrderByExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

instance Semigroup OrderByExpr where
  (<>) = appendOrderByExpr

appendOrderByExpr :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderByExpr (OrderByExpr a) (OrderByExpr b) =
  OrderByExpr (a <> RawSql.commaSpace <> b)

orderByExpr :: RawSql.RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql orderSql =
  OrderByExpr $ sql <> RawSql.space <> RawSql.toRawSql orderSql

orderByColumnsExpr :: NonEmpty (ColumnName, OrderByDirection) -> OrderByExpr
orderByColumnsExpr columns =
  OrderByExpr . RawSql.intercalate RawSql.commaSpace . NE.map columnOrdering $ columns
  where
    columnOrdering :: (ColumnName, OrderByDirection) -> RawSql.RawSql
    columnOrdering (columnName, orderByDirection) =
      RawSql.toRawSql columnName <> RawSql.space <> RawSql.toRawSql orderByDirection

{-- |
  Orders a query by the given column name in the given order direction.
-}
orderByColumnName :: ColumnName -> OrderByDirection -> OrderByExpr
orderByColumnName =
  orderByExpr . RawSql.toRawSql
