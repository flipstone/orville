{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
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
Type to represent a SQL query, E.G.

> SELECT id FROM some_table

'QueryExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Builds a 'QueryExpr' from the given 'SelectClause', 'SelectList' and
  'TableExpr'. The resulting 'QueryExpr' is suitable execution via the SQL
  execution functions in "Orville.PostgreSQL.Execution" and
  "Orville.PostgreSQL.Raw.RawSql".

@since 0.10.0.0
-}
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
Type to represent the list of items to be selected in a @SELECT@ clause.
E.G. the

> foo, bar, baz

in

> SELECT foo, bar, baz FROM some_table

'SelectList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype SelectList = SelectList RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SelectList' that will select all colums (i.e. the @*@ in
  @SELECT *@").

  @since 0.10.0.0
-}
selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

{- |
  Constructs a 'SelectList' that will select the specified column names. This
  is a special case of 'selectDerivedColumns' where all the items to be
  selected are simple column references.

  @since 0.10.0.0
-}
selectColumns :: [ColumnName] -> SelectList
selectColumns =
  selectDerivedColumns . map (deriveColumn . columnReference)

{- |
Type to represent an individual item in a list of selected items. E.G.

> now() as current_time

'DerivedColumn' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype DerivedColumn = DerivedColumn RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'SelectList' that will select the specified items, which may be
  column references or other expressions as allowed by 'DerivedColumn'. See
  also 'selectColumns' the simpler case of selecting a list of column names.

@since 0.10.0.0
-}
selectDerivedColumns :: [DerivedColumn] -> SelectList
selectDerivedColumns =
  SelectList . RawSql.intercalate RawSql.comma

{- |
  Constructs a 'DerivedColumn' that will select the given value. No name will
  be given to the value in the result set. See 'deriveColumnAs' to give the
  value a name in the result set.

@since 0.10.0.0
-}
deriveColumn :: ValueExpression -> DerivedColumn
deriveColumn =
  DerivedColumn . RawSql.toRawSql

{- |
  Constructs a 'DerivedColumn' that will select the given value and give it
  the specified column name in the result set.

@since 0.10.0.0
-}
deriveColumnAs :: ValueExpression -> ColumnName -> DerivedColumn
deriveColumnAs valueExpr asColumn =
  DerivedColumn
    ( RawSql.toRawSql valueExpr
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asColumn
    )

{- |
Type to represent a table expression (including its associated options) in a
@SELECT@. This is the part that would appear *after* the word @FROM@. E.G.

> foo
> WHERE id > 100
> ORDER BY id
> LIMIT 1
> OFFSET 2

'TableExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 0.10.0.0
-}
newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- |
  Constructs a 'TableExpr' with the given options.

@since 0.10.0.0
-}
tableExpr ::
  -- | The list of tables to query from
  TableReferenceList ->
  -- | An optional @WHERE@ clause to limit the results returned
  Maybe WhereClause ->
  -- | An optional @GROUP BY@ clause to group the result set rows
  Maybe GroupByClause ->
  -- | An optional @ORDER BY@ clause to order the result set rows
  Maybe OrderByClause ->
  -- | An optional @LIMIT@ to apply to the result set
  Maybe LimitExpr ->
  -- | An optional @OFFSET@ to apply to the result set
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
