{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Query
  ( QueryExpr
  , queryExpr
  , SelectList
  , selectColumns
  , DerivedColumn
  , deriveColumn
  , deriveColumnAs
  , selectDerivedColumns
  , selectStar
  , TableExpr
  , tableExpr
  )
where

import Data.Maybe (catMaybes, fromMaybe)

import Orville.PostgreSQL.Expr.GroupBy (GroupByClause)
import Orville.PostgreSQL.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Expr.Name (ColumnName)
import Orville.PostgreSQL.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Expr.OrderBy (OrderByClause)
import Orville.PostgreSQL.Expr.Select (SelectClause)
import Orville.PostgreSQL.Expr.TableReferenceList (TableReferenceList)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, columnReference)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
{- |
Type to represent a SQL query -- e.g. a @SELECT@ statement.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a query by hand
and use it in a place that expected a 'QueryExpr', that could be done as

 > RawSql.unsafeSqlExpression "SELECT 1"

@since 0.10.0.0
-}
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

queryExpr :: SelectClause -> SelectList -> Maybe TableExpr -> QueryExpr
queryExpr querySelectClause selectList maybeTableExpr =
  let
    maybeFromClause = do
      table <- maybeTableExpr
      pure (RawSql.fromString " FROM " <> RawSql.toRawSql table)
  in
    QueryExpr $
      mconcat
        [ RawSql.toRawSql querySelectClause
        , RawSql.toRawSql selectList
        , fromMaybe (RawSql.fromString "") maybeFromClause
        ]

{- |
Type to represent a select use for a SQL query.

There is an low level escape hatch included here, by means of the instance of
'RawSql.SqlExpression'. This is intended to be used when some functionality is
required but not already included. The exension mechanism provided does require
care in use as no guarantees are provided for correctness in usage.

For example, if one wanted to write a select list by hand
and use it in a place that expected a 'SelectList', that could be done as

 > RawSql.unsafeSqlExpression "foo,bar,baz"

@since 0.10.0.0
-}
newtype SelectList = SelectList RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

selectColumns :: [ColumnName] -> SelectList
selectColumns =
  selectDerivedColumns . map (deriveColumn . columnReference)

newtype DerivedColumn = DerivedColumn RawSql.RawSql
  deriving (RawSql.SqlExpression)

selectDerivedColumns :: [DerivedColumn] -> SelectList
selectDerivedColumns =
  SelectList . RawSql.intercalate RawSql.comma

deriveColumn :: ValueExpression -> DerivedColumn
deriveColumn =
  DerivedColumn . RawSql.toRawSql

deriveColumnAs :: ValueExpression -> ColumnName -> DerivedColumn
deriveColumnAs valueExpr asColumn =
  DerivedColumn
    ( RawSql.toRawSql valueExpr
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asColumn
    )

newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

tableExpr ::
  TableReferenceList ->
  Maybe WhereClause ->
  Maybe GroupByClause ->
  Maybe OrderByClause ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  TableExpr
tableExpr
  tableReferenceList
  maybeWhereClause
  maybeGroupByClause
  maybeOrderByClause
  maybeLimitExpr
  maybeOffsetExpr =
    TableExpr
      . RawSql.intercalate RawSql.space
      $ RawSql.toRawSql tableReferenceList
        : catMaybes
          [ RawSql.toRawSql <$> maybeWhereClause
          , RawSql.toRawSql <$> maybeGroupByClause
          , RawSql.toRawSql <$> maybeOrderByClause
          , RawSql.toRawSql <$> maybeLimitExpr
          , RawSql.toRawSql <$> maybeOffsetExpr
          ]
