{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
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
  , orderByColumnsExpr
  , orderByAlias
  , orderByAliasesExpr
  , OrderByDirection
  , NullsOrder (NullsFirst, NullsLast)
  , ascendingOrder
  , descendingOrder
  , ascendingOrderWith
  , descendingOrderWith
  )
where

import qualified Data.List.NonEmpty as NEL

import Orville.PostgreSQL.Expr.Name (AliasExpr, ColumnName, Qualified)
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

{- |
Builds a full 'OrderByClause' with the given ordering described in the 'OrderByExpr'

@since 1.0.0.0
-}
orderByClause :: OrderByExpr -> OrderByClause
orderByClause expr = OrderByClause (RawSql.fromString "ORDER BY " <> RawSql.toRawSql expr)

{- |
Type to represent a SQL order by expression (the part that follows the @ORDER
BY@ in SQL). E.G.

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

{- | Create an 'OrderByExpr' for 'ColumnName' and 'OrderByDirection' pairs, ensuring commas as
  needed.

@since 1.0.0.0
-}
orderByColumnsExpr :: NEL.NonEmpty (Qualified ColumnName, OrderByDirection) -> OrderByExpr
orderByColumnsExpr =
  OrderByExpr . RawSql.intercalate RawSql.commaSpace . fmap columnOrdering
 where
  columnOrdering :: (Qualified ColumnName, OrderByDirection) -> RawSql.RawSql
  columnOrdering (columnName, orderByDirection) =
    RawSql.toRawSql columnName <> RawSql.space <> RawSql.toRawSql orderByDirection

{- |
  Orders a query by the given column name in the given order direction.

@since 1.0.0.0
-}
orderByColumnName :: Qualified ColumnName -> OrderByDirection -> OrderByExpr
orderByColumnName =
  curry (orderByColumnsExpr . pure)

{- | Create an 'OrderByExpr' for 'AliasExpr' and 'OrderByDirection' pairs, ensuring commas as needed. For basic queries involving columns on some table(s), use 'orderByColumnsExpr'.

@since 1.1.0.0
-}
orderByAliasesExpr :: NEL.NonEmpty (AliasExpr, OrderByDirection) -> OrderByExpr
orderByAliasesExpr =
  OrderByExpr . RawSql.intercalate RawSql.commaSpace . fmap columnOrdering
 where
  columnOrdering :: (AliasExpr, OrderByDirection) -> RawSql.RawSql
  columnOrdering (alias, orderByDirection) =
    RawSql.toRawSql alias <> RawSql.space <> RawSql.toRawSql orderByDirection

{- |
  Orders a query by the given alias in the given order direction. This is useful for ordering by synthetic fields or sub queries.

@since 1.1.0.0
-}
orderByAlias :: AliasExpr -> OrderByDirection -> OrderByExpr
orderByAlias =
  curry (orderByAliasesExpr . pure)

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
