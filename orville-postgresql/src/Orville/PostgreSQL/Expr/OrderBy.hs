{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
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

import Orville.PostgreSQL.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- |
Type to represent a SQL order by clause. E.G.

> ORDER BY foo, bar

'OrderByClause' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype OrderByClause
  = OrderByClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> RawSql.toRawSql expr)

{- |
Type to represent a SQL order by expression (the part that follows the @ORDER
BY@ in sql). E.G.

> foo, bar

'OrderByExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype OrderByExpr = OrderByExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
@since 1.0.0.0
-}
instance Semigroup OrderByExpr where
  (<>) = appendOrderByExpr

{- | Combines two 'OrderByExpr's with a comma between them.

@since 1.0.0.0
-}
appendOrderByExpr :: OrderByExpr -> OrderByExpr -> OrderByExpr
appendOrderByExpr (OrderByExpr a) (OrderByExpr b) =
  OrderByExpr (a <> RawSql.commaSpace <> b)

{- | Create a 'OrderByExpr' from some 'RawSql' and a 'OrderByDirection'. Note that it is up to the
  caller to ensure that the given value can actually be used for a 'OrderByExpr'

@since 1.0.0.0
-}
orderByExpr :: RawSql.RawSql -> OrderByDirection -> OrderByExpr
orderByExpr sql orderSql =
  OrderByExpr $ sql <> RawSql.space <> RawSql.toRawSql orderSql

{- | Create a 'OrderByExpr' for 'ColumnName' and 'OrderByDirection' pairs, ensuring commas as
  needed.

@since 1.0.0.0
-}
orderByColumnsExpr :: NonEmpty (ColumnName, OrderByDirection) -> OrderByExpr
orderByColumnsExpr =
  OrderByExpr . RawSql.intercalate RawSql.commaSpace . fmap columnOrdering
 where
  columnOrdering :: (ColumnName, OrderByDirection) -> RawSql.RawSql
  columnOrdering (columnName, orderByDirection) =
    RawSql.toRawSql columnName <> RawSql.space <> RawSql.toRawSql orderByDirection

{-- |
  Orders a query by the given column name in the given order direction.

@since 1.0.0.0
-}
orderByColumnName :: ColumnName -> OrderByDirection -> OrderByExpr
orderByColumnName =
  orderByExpr . RawSql.toRawSql

{- |
Type to represent a SQL order by direction expression. E.G.

> ASC

'OrderByDirection' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype OrderByDirection = OrderByDirection RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
Type to represent the ordering of Null, intended to be used with 'OrderByDirection'.

@since 1.0.0.0
-}
data NullsOrder
  = NullsFirst
  | NullsLast
  deriving
    ( -- | @since 1.0.0.0
      Eq
    , -- | @since 1.0.0.0
      Show
    , -- | @since 1.0.0.0
      Ord
    , -- | @since 1.0.0.0
      Enum
    , -- | @since 1.0.0.0
      Bounded
    )

{- | The SQL ASC order direction.

@since 1.0.0.0
-}
ascendingOrder :: OrderByDirection
ascendingOrder = OrderByDirection $ RawSql.fromString "ASC"

{- | The SQL DESC order direction.

@since 1.0.0.0
-}
descendingOrder :: OrderByDirection
descendingOrder = OrderByDirection $ RawSql.fromString "DESC"

{- | The SQL ASC order direction with NULLs ordered as given.

@since 1.0.0.0
-}
ascendingOrderWith :: NullsOrder -> OrderByDirection
ascendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql ascendingOrder <> RawSql.space <> nullsOrder no

{- | The SQL DESC order direction with NULLs ordered as given.

@since 1.0.0.0
-}
descendingOrderWith :: NullsOrder -> OrderByDirection
descendingOrderWith no =
  OrderByDirection $ RawSql.toRawSql descendingOrder <> RawSql.space <> nullsOrder no

nullsOrder :: NullsOrder -> RawSql.RawSql
nullsOrder no = case no of
  NullsFirst -> RawSql.fromString "NULLS FIRST"
  NullsLast -> RawSql.fromString "NULLS LAST"
