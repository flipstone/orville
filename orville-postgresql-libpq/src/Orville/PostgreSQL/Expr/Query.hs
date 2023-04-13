{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Query
  ( QueryExpr,
    queryExpr,
    SelectList,
    selectColumns,
    DerivedColumn,
    deriveColumn,
    deriveColumnAs,
    selectDerivedColumns,
    selectStar,
    TableExpr,
    tableExpr,
  )
where

import Data.Maybe (catMaybes, fromMaybe)

import Orville.PostgreSQL.Expr.GroupBy (GroupByClause)
import Orville.PostgreSQL.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Expr.Name (ColumnName, Qualified, TableName)
import Orville.PostgreSQL.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Expr.OrderBy (OrderByClause)
import Orville.PostgreSQL.Expr.Select (SelectClause)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, columnReference)
import Orville.PostgreSQL.Expr.WhereClause (WhereClause)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

queryExpr :: SelectClause -> SelectList -> Maybe TableExpr -> QueryExpr
queryExpr querySelectClause selectList maybeTableExpr =
  let maybeFromClause = do
        table <- maybeTableExpr
        pure (RawSql.fromString " FROM " <> RawSql.toRawSql table)
   in QueryExpr $
        mconcat
          [ RawSql.toRawSql querySelectClause
          , RawSql.toRawSql selectList
          , fromMaybe (RawSql.fromString "") maybeFromClause
          ]

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
  DerivedColumn $
    ( RawSql.toRawSql valueExpr
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asColumn
    )

newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

tableExpr ::
  Qualified TableName ->
  Maybe WhereClause ->
  Maybe OrderByClause ->
  Maybe GroupByClause ->
  Maybe LimitExpr ->
  Maybe OffsetExpr ->
  TableExpr
tableExpr
  tableName
  maybeWhereClause
  maybeOrderByClause
  maybeGroupByClause
  maybeLimitExpr
  maybeOffsetExpr =
    TableExpr $
      RawSql.intercalate RawSql.space $
        (RawSql.toRawSql tableName) :
        catMaybes
          [ RawSql.toRawSql <$> maybeWhereClause
          , RawSql.toRawSql <$> maybeOrderByClause
          , RawSql.toRawSql <$> maybeGroupByClause
          , RawSql.toRawSql <$> maybeLimitExpr
          , RawSql.toRawSql <$> maybeOffsetExpr
          ]
