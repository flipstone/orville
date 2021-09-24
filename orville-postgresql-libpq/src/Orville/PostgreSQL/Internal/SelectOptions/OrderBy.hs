{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.SelectOptions.OrderBy
  ( OrderBy,
    Expr.OrderByDirection,
    Expr.ascendingOrder,
    Expr.descendingOrder,
    orderByOrderByExpr,
    orderByField,
    orderByColumnName,
    appendOrderBy,
    orderByToClause,
    orderByToExpr,
  )
where

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.FieldDefinition as FieldDef
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

{-- |
  An 'OrderBy' represents a SQL expression that can be added to SELECT
  statements to control the ordering of rows returned by a query. Various
  functions are provided below to construct orderings.
-}
newtype OrderBy
  = OrderBy Expr.OrderByExpr
  deriving (RawSql.SqlExpression)

instance Semigroup OrderBy where
  (<>) = appendOrderBy

{-- |
  Constructs an 'OrderBy' from an 'Expr.OrderByExpr'. You can use this together
  with the 'RawSql.RawSql' related functions in the 'Expr' module to use SQL
  expressions that Orville does not directly support in your queries.
-}
orderByOrderByExpr :: Expr.OrderByExpr -> OrderBy
orderByOrderByExpr =
  OrderBy

{-- |
  Builds an 'Expr.OrderByClause' to be used when executing a query with the
  given 'OrderBy'

  Note: 'Expr.OrderByClause' represents a query fragment with the @ORDER BY@
  included in it. You can use 'orderByToExpr' instead if you need the
  order expression without the @ORDER BY@ clause included.
--}
orderByToClause :: OrderBy -> Expr.OrderByClause
orderByToClause =
  Expr.orderByClause . orderByToExpr

{-- |
  Builds an 'Expr.OrderByExpr to be used when executing a query with the
  given 'OrderBy'

  Note: 'Expr.OrderByClause' represents a query fragment without the @ORDER BY@
  included in it. You can use 'orderByToClause' instead if you need the
  order expression with the @ORDER BY@ clause included.
--}
orderByToExpr :: OrderBy -> Expr.OrderByExpr
orderByToExpr (OrderBy expr) =
  expr

{-- |
  Orders a query by the given column name in the given order direction.
-}
orderByColumnName :: Expr.ColumnName -> Expr.OrderByDirection -> OrderBy
orderByColumnName name =
  orderByOrderByExpr . Expr.orderByExpr (RawSql.toRawSql name)

{-- |
  Orders a query by the column name for the given field.
--}
orderByField ::
  FieldDef.FieldDefinition nullability value ->
  Expr.OrderByDirection ->
  OrderBy
orderByField =
  orderByColumnName . FieldDef.fieldColumnName

{-- |
  Appends two 'OrderBy' values so that the resulting query will order
  by the first expression, then the second. This function is also available
  as the implementation of @(<>)@ for the 'Semigroup' instance of 'OrderBy'.
--}
appendOrderBy :: OrderBy -> OrderBy -> OrderBy
appendOrderBy (OrderBy expr1) (OrderBy expr2) =
  OrderBy (Expr.appendOrderByExpr expr1 expr2)
