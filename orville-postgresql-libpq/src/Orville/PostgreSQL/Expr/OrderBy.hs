{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.OrderBy
  ( OrderByClause
  , orderByClause
  , OrderByExpr
  , appendOrderByExpr
  , orderByColumnName
  , orderByExpr
  , orderByColumnsExpr
  , OrderByDirection
  , NullsOrder (NullsFirst, NullsLast)
  , ascendingOrder
  , descendingOrder
  , ascendingOrderWith
  , descendingOrderWith
  )
where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype OrderByClause
  = OrderByClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> RawSql.toRawSql expr)

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

newtype OrderByDirection = OrderByDirection RawSql.RawSql
  deriving (RawSql.SqlExpression)

data NullsOrder
  = NullsFirst
  | NullsLast
  deriving (Eq, Show, Ord, Enum, Bounded)

ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"

ascendingOrderWith :: NullsOrder -> OrderByDirection
ascendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql ascendingOrder <> RawSql.space <> nullsOrder no

descendingOrderWith :: NullsOrder -> OrderByDirection
descendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql descendingOrder <> RawSql.space <> nullsOrder no

nullsOrder :: NullsOrder -> RawSql.RawSql
nullsOrder no = case no of
  NullsFirst -> RawSql.fromString "NULLS FIRST"
  NullsLast -> RawSql.fromString "NULLS LAST"
