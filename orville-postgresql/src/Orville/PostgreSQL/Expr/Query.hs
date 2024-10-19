{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Query
  ( QueryExpr
  , queryExpr
  , existsSubquery
  , queryValueExpression
  , inSubquery
  , notInSubquery
  , anySubquery
  , allSubquery
  , queryExprWithAlias
  , joinQueryExpr
  , subQueryAsFromItemExpr
  , SelectList
  , selectColumns
  , DerivedColumn
  , deriveColumn
  , deriveColumnAs
  , deriveColumnAsAlias
  , selectDerivedColumns
  , selectStar
  , TableExpr
  , tableExpr
  )
where

import Data.Maybe (catMaybes, fromMaybe)

import Orville.PostgreSQL.Expr.BinaryOperator (BinaryOperator)
import Orville.PostgreSQL.Expr.FromItemExpr (FromItemExpr)
import Orville.PostgreSQL.Expr.GroupBy (GroupByClause)
import Orville.PostgreSQL.Expr.Join (JoinConstraint, JoinExpr, JoinType, joinExpr)
import Orville.PostgreSQL.Expr.LimitExpr (LimitExpr)
import Orville.PostgreSQL.Expr.Name (AliasExpr, ColumnName, Qualified)
import Orville.PostgreSQL.Expr.OffsetExpr (OffsetExpr)
import Orville.PostgreSQL.Expr.OrderBy (OrderByClause)
import Orville.PostgreSQL.Expr.RowLocking (RowLockingClause)
import Orville.PostgreSQL.Expr.Select (SelectClause)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, columnReference)
import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr, InValuePredicate, WhereClause, inPredicate, notInPredicate)
import Orville.PostgreSQL.Expr.Window (WindowClause)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

-- This is a rough model of "query specification" see https://jakewheat.github.io/sql-overview/sql-2016-foundation-grammar.html#_7_16_query_specification for more detail than you probably want

{- |
Type to represent a SQL query, E.G.

> SELECT id FROM some_table

'QueryExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype QueryExpr
  = QueryExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  Builds a 'QueryExpr' from the given 'SelectClause', 'SelectList' and
  'TableExpr'. The resulting 'QueryExpr' is suitable for execution via the SQL
  execution functions in "Orville.PostgreSQL.Execution" and
  "Orville.PostgreSQL.Raw.RawSql".

@since 1.0.0.0
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

{- | The SQL @EXISTS@ subquery expression. This builds a 'BooleanExpr' that checks if the given
   'QueryExpr' returns at least one row. The resuling SQL will correctly be parenthesized.

@since 1.1.0.0
-}
existsSubquery :: QueryExpr -> BooleanExpr
existsSubquery (QueryExpr queryRawSql) =
  RawSql.unsafeFromRawSql $
    RawSql.fromString "EXISTS "
      <> RawSql.parenthesized queryRawSql

{- | Treat a 'QueryExpr' as a 'ValueExpression' to make it suitable for use as subquery.

@since 1.1.0.0
-}
queryValueExpression :: QueryExpr -> ValueExpression
queryValueExpression =
  RawSql.unsafeFromRawSql . RawSql.toRawSql

-- Internal helper to treat a 'QueryExpr' as an 'InValuePredicate' for building @IN@ and @NOT IN@
-- subquery expressions.
queryInValuePredicate :: QueryExpr -> InValuePredicate
queryInValuePredicate =
  RawSql.unsafeFromRawSql . RawSql.parenthesized . RawSql.toRawSql

{- | The SQL @IN@ subquery expression. It is up to the caller to ensure that the given 'QueryExpr'
   returns exactly one column.

@since 1.1.0.0
-}
inSubquery :: ValueExpression -> QueryExpr -> BooleanExpr
inSubquery valExpr =
  inPredicate valExpr . queryInValuePredicate

{- | The SQL @NOT IN@ subquery expression. It is up to the caller to ensure that the given 'QueryExpr'
   returns exactly one column.

@since 1.1.0.0
-}
notInSubquery :: ValueExpression -> QueryExpr -> BooleanExpr
notInSubquery valExpr =
  notInPredicate valExpr . queryInValuePredicate

{- | The SQL @ANY@ subquery expression. It is up to the caller to ensure that the given 'QueryExpr'
   returns exactly one column and that the operator results in a boolean.

@since 1.1.0.0
-}
anySubquery :: BinaryOperator -> ValueExpression -> QueryExpr -> BooleanExpr
anySubquery =
  operatorSubquery (RawSql.fromString " ANY ")

{- | The SQL @ALL@ subquery expression. It is up to the caller to ensure that the given 'QueryExpr'
   returns exactly one column and that the operator results in a boolean.

@since 1.1.0.0
-}
allSubquery :: BinaryOperator -> ValueExpression -> QueryExpr -> BooleanExpr
allSubquery =
  operatorSubquery (RawSql.fromString " ALL ")

-- Internal helper to build any/all subquery
operatorSubquery :: RawSql.RawSql -> BinaryOperator -> ValueExpression -> QueryExpr -> BooleanExpr
operatorSubquery querySpecificRawSql binOp valExpr (QueryExpr queryRawSql) =
  RawSql.unsafeFromRawSql $
    RawSql.parenthesized valExpr
      <> RawSql.space
      <> RawSql.toRawSql binOp
      <> querySpecificRawSql
      <> RawSql.parenthesized queryRawSql

{- | Append the SQL @AS@ and given 'AliasExpr' to a 'QueryExpr'

@since 1.1.0.0
-}
queryExprWithAlias :: AliasExpr -> QueryExpr -> QueryExpr
queryExprWithAlias alias query =
  QueryExpr $
    (RawSql.parenthesized $ RawSql.toRawSql query)
      <> RawSql.fromString " AS "
      <> RawSql.toRawSql alias

{- | Make a 'QueryExpr' into a 'FromItemExpr', aliased appropriately, so that it can be used to
   as a subquery in @SELECT@ion.

@since 1.1.0.0
-}
subQueryAsFromItemExpr :: AliasExpr -> QueryExpr -> FromItemExpr
subQueryAsFromItemExpr alias =
  RawSql.unsafeFromRawSql . RawSql.toRawSql . queryExprWithAlias alias

{- | Build a 'JoinExpr' from a 'QueryExpr' with the given options

@since 1.1.0.0
-}
joinQueryExpr ::
  -- | The type of SQL @JOIN@ to perform
  JoinType ->
  -- | The alias the subquery should have
  AliasExpr ->
  -- | The subquery we wish to join with
  QueryExpr ->
  -- | What conditions will be joined on.
  JoinConstraint ->
  JoinExpr
joinQueryExpr joinType alias qexpr =
  joinExpr joinType (subQueryAsFromItemExpr alias qexpr)

{- |
Type to represent the list of items to be selected in a @SELECT@ clause.
E.G. the

> foo, bar, baz

in

> SELECT foo, bar, baz FROM some_table

'SelectList' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype SelectList = SelectList RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

-- | @since 1.1.0.0
instance Semigroup SelectList where
  SelectList a <> SelectList b =
    SelectList $ RawSql.appendWithCommaSpace a b

{- |
  Constructs a 'SelectList' that will select all colums (i.e. the @*@ in
  @SELECT *@").

  @since 1.0.0.0
-}
selectStar :: SelectList
selectStar =
  SelectList (RawSql.fromString "*")

{- |
  Constructs a 'SelectList' that will select the specified column names. This
  is a special case of 'selectDerivedColumns' where all the items to be
  selected are simple column references.

  @since 1.0.0.0
-}
selectColumns :: [Qualified ColumnName] -> SelectList
selectColumns =
  selectDerivedColumns . map (deriveColumn . columnReference)

{- |
Type to represent an individual item in a list of selected items. E.G.

> now() as current_time

'DerivedColumn' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.0.0.0
-}
newtype DerivedColumn = DerivedColumn RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs a 'SelectList' that will select the specified items, which may be
  column references or other expressions as allowed by 'DerivedColumn'. See
  also 'selectColumns' the simpler case of selecting a list of column names.

@since 1.0.0.0
-}
selectDerivedColumns :: [DerivedColumn] -> SelectList
selectDerivedColumns =
  SelectList . RawSql.intercalate RawSql.comma

{- |
  Constructs a 'DerivedColumn' that will select the given value. No name will
  be given to the value in the result set. See 'deriveColumnAs' to give the
  value a name in the result set.

@since 1.0.0.0
-}
deriveColumn :: ValueExpression -> DerivedColumn
deriveColumn =
  DerivedColumn . RawSql.toRawSql

{- |
  Constructs a 'DerivedColumn' that will select the given value and give it
  the specified column name in the result set.

@since 1.0.0.0
-}
deriveColumnAs :: ValueExpression -> ColumnName -> DerivedColumn
deriveColumnAs valueExpr asColumn =
  DerivedColumn
    ( RawSql.toRawSql valueExpr
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asColumn
    )

{- |
  Constructs a 'DerivedColumn' that will select the given value and give it
  the specified alias in the result set.

@since 1.1.0.0
-}
deriveColumnAsAlias :: ValueExpression -> AliasExpr -> DerivedColumn
deriveColumnAsAlias valueExpr asAlias =
  DerivedColumn
    ( RawSql.toRawSql valueExpr
        <> RawSql.fromString " AS "
        <> RawSql.toRawSql asAlias
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

@since 1.0.0.0
-}
newtype TableExpr
  = TableExpr RawSql.RawSql
  deriving
    ( -- | @since 1.0.0.0
      RawSql.SqlExpression
    )

{- |
  Constructs a 'TableExpr' with the given options.

@since 1.0.0.0
-}
tableExpr ::
  -- | The item(s) to query from.
  FromItemExpr ->
  -- | An optional @WHERE@ clause to limit the results returned.
  Maybe WhereClause ->
  -- | An optional @GROUP BY@ clause to group the result set rows.
  Maybe GroupByClause ->
  -- | An optional @ORDER BY@ clause to order the result set rows.
  Maybe OrderByClause ->
  -- | An optional @LIMIT@ to apply to the result set.
  Maybe LimitExpr ->
  -- | An optional @OFFSET@ to apply to the result set.
  Maybe OffsetExpr ->
  -- | An optional locking clause to apply to the result set.
  Maybe RowLockingClause ->
  -- | An optional @WINDOW@ clause to apply to the result set.
  Maybe WindowClause ->
  TableExpr
tableExpr
  tableReferenceList
  maybeWhereClause
  maybeGroupByClause
  maybeOrderByClause
  maybeLimitExpr
  maybeOffsetExpr
  maybeRowLockingClause
  maybeWindowClause =
    TableExpr
      . RawSql.intercalate RawSql.space
      $ RawSql.toRawSql tableReferenceList
        : catMaybes
          [ RawSql.toRawSql <$> maybeWhereClause
          , RawSql.toRawSql <$> maybeGroupByClause
          , RawSql.toRawSql <$> maybeWindowClause
          , RawSql.toRawSql <$> maybeOrderByClause
          , RawSql.toRawSql <$> maybeLimitExpr
          , RawSql.toRawSql <$> maybeOffsetExpr
          , RawSql.toRawSql <$> maybeRowLockingClause
          ]
