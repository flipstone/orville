{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Execution.SelectOptions
  ( SelectOptions
  , emptySelectOptions
  , appendSelectOptions
  , selectDistinct
  , selectWhereClause
  , selectOrderByClause
  , selectGroupByClause
  , selectLimitExpr
  , selectOffsetExpr
  , selectRowLockingClause
  , selectWindowClause
  , selectFetchClause
  , distinct
  , where_
  , orderBy
  , limit
  , offset
  , groupBy
  , forRowLock
  , window
  , fetchRow
  , selectOptionsQueryExpr
  )
where

import Data.Monoid (First (First, getFirst))

import qualified Orville.PostgreSQL.Expr as Expr

{- |
   A 'SelectOptions' is a set of options that can be used to change the way
   a basic query function works by adding @WHERE@, @ORDER BY@, @GROUP BY@, etc.
   Functions are provided to construct 'SelectOptions' for individual options,
   which may then be combined via '<>' (also exposed as 'appendSelectOptions').

@since 1.0.0.0
-}
data SelectOptions = SelectOptions
  { i_distinct :: First Bool
  , i_whereCondition :: Maybe Expr.BooleanExpr
  , i_orderBy :: Maybe Expr.OrderByExpr
  , i_limitExpr :: First Expr.LimitExpr
  , i_offsetExpr :: First Expr.OffsetExpr
  , i_groupByExpr :: Maybe Expr.GroupByExpr
  , i_rowLockingClause :: First Expr.RowLockingClause
  , i_windowExpr :: Maybe Expr.NamedWindowDefinitionExpr
  , i_fetchClause :: First Expr.FetchClause
  }

-- | @since 1.0.0.0
instance Semigroup SelectOptions where
  (<>) = appendSelectOptions

-- | @since 1.0.0.0
instance Monoid SelectOptions where
  mempty = emptySelectOptions

{- |
  A set of empty 'SelectOptions' that will not change how a query is run.

@since 1.0.0.0
-}
emptySelectOptions :: SelectOptions
emptySelectOptions =
  SelectOptions
    { i_distinct = mempty
    , i_whereCondition = Nothing
    , i_orderBy = mempty
    , i_limitExpr = mempty
    , i_offsetExpr = mempty
    , i_groupByExpr = mempty
    , i_rowLockingClause = mempty
    , i_windowExpr = Nothing
    , i_fetchClause = mempty
    }

{- |
  Combines multple select options together, unioning the options together where
  possible. For options where this is not possible (e.g. @LIMIT@), the one
  on the left is preferred.

@since 1.0.0.0
-}
appendSelectOptions :: SelectOptions -> SelectOptions -> SelectOptions
appendSelectOptions left right =
  SelectOptions
    (i_distinct left <> i_distinct right)
    (unionMaybeWith Expr.andExpr (i_whereCondition left) (i_whereCondition right))
    (i_orderBy left <> i_orderBy right)
    (i_limitExpr left <> i_limitExpr right)
    (i_offsetExpr left <> i_offsetExpr right)
    (i_groupByExpr left <> i_groupByExpr right)
    (i_rowLockingClause left <> i_rowLockingClause right)
    (i_windowExpr left <> i_windowExpr right)
    (i_fetchClause left <> i_fetchClause right)

unionMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
unionMaybeWith f mbLeft mbRight =
  case (mbLeft, mbRight) of
    (Nothing, Nothing) -> Nothing
    (Just left, Nothing) -> Just left
    (Nothing, Just right) -> Just right
    (Just left, Just right) -> Just (f left right)

{- |
  Builds the 'Expr.SelectClause' that should be used to include the
  'distinct's from the 'SelectOptions' on a query.

@since 1.0.0.0
-}
selectDistinct :: SelectOptions -> Expr.SelectClause
selectDistinct selectOptions =
  case i_distinct selectOptions of
    First (Just True) -> Expr.selectClause . Expr.selectExpr $ Just Expr.Distinct
    _ -> Expr.selectClause $ Expr.selectExpr Nothing

{- |
  Builds the 'Expr.WhereClause' that should be used to include the
  'Expr.BooleanExpr's from the 'SelectOptions' on a query. This will be 'Nothing'
  when no 'Expr.BooleanExpr's have been specified.

@since 1.0.0.0
-}
selectWhereClause :: SelectOptions -> Maybe Expr.WhereClause
selectWhereClause =
  fmap Expr.whereClause . i_whereCondition

{- |
  Constructs a 'SelectOptions' with just 'distinct' set to 'True'.

@since 1.0.0.0
-}
distinct :: SelectOptions
distinct =
  emptySelectOptions
    { i_distinct = First $ Just True
    }

{- |
  Builds the 'Expr.OrderByClause' that should be used to include the
  'Expr.OrderByClause's from the 'SelectOptions' on a query. This will be
  'Nothing' when no 'Expr.OrderByClause's have been specified.

@since 1.0.0.0
-}
selectOrderByClause :: SelectOptions -> Maybe Expr.OrderByClause
selectOrderByClause =
  fmap Expr.orderByClause . i_orderBy

{- |
  Builds the 'Expr.GroupByClause' that should be used to include the
  'Expr.GroupByClause's from the 'SelectOptions' on a query. This will be
  'Nothing' when no 'Expr.GroupByClause's have been specified.

@since 1.0.0.0
-}
selectGroupByClause :: SelectOptions -> Maybe Expr.GroupByClause
selectGroupByClause =
  fmap Expr.groupByClause . i_groupByExpr

{- |
  Builds a 'Expr.LimitExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any).

@since 1.0.0.0
-}
selectLimitExpr :: SelectOptions -> Maybe Expr.LimitExpr
selectLimitExpr =
  getFirst . i_limitExpr

{- |
  Builds an 'Expr.OffsetExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any).

@since 1.0.0.0
-}
selectOffsetExpr :: SelectOptions -> Maybe Expr.OffsetExpr
selectOffsetExpr =
  getFirst . i_offsetExpr

{- |
  Builds an 'Expr.RowLockingClause' that will apply the locking rules specified in the 'SelectOptions' (if any).

@since 1.1.0.0
-}
selectRowLockingClause :: SelectOptions -> Maybe Expr.RowLockingClause
selectRowLockingClause =
  getFirst . i_rowLockingClause

{- |
  Builds an 'Expr.WindowClause' that will apply the windowing rules specified in the 'SelectOptions' (if any).

@since 1.1.0.0
-}
selectWindowClause :: SelectOptions -> Maybe Expr.WindowClause
selectWindowClause =
  fmap Expr.windowClause . i_windowExpr

{- |
  Builds an 'Expr.FetchClause' that will apply the fetching rules specified in the 'SelectOptions' (if any).

@since 1.1.0.0
-}
selectFetchClause :: SelectOptions -> Maybe Expr.FetchClause
selectFetchClause =
  getFirst . i_fetchClause

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.BooleanExpr'.

@since 1.0.0.0
-}
where_ :: Expr.BooleanExpr -> SelectOptions
where_ condition =
  emptySelectOptions
    { i_whereCondition = Just condition
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.OrderByExpr'.

@since 1.0.0.0
-}
orderBy :: Expr.OrderByExpr -> SelectOptions
orderBy order =
  emptySelectOptions
    { i_orderBy = Just order
    }

{- |
  Constructs a 'SelectOptions' that will apply the given limit.

@since 1.0.0.0
-}
limit :: Int -> SelectOptions
limit limitValue =
  emptySelectOptions
    { i_limitExpr = First . Just . Expr.limitExpr $ limitValue
    }

{- |
  Constructs a 'SelectOptions' that will apply the given offset.

@since 1.0.0.0
-}
offset :: Int -> SelectOptions
offset offsetValue =
  emptySelectOptions
    { i_offsetExpr = First . Just . Expr.offsetExpr $ offsetValue
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.GroupByExpr'.

@since 1.0.0.0
-}
groupBy :: Expr.GroupByExpr -> SelectOptions
groupBy groupByExpr =
  emptySelectOptions
    { i_groupByExpr = Just groupByExpr
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.RowLockingClause'.

@since 1.1.0.0
-}
forRowLock :: Expr.RowLockingClause -> SelectOptions
forRowLock rowLockingClause =
  emptySelectOptions
    { i_rowLockingClause = First $ Just rowLockingClause
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.NamedWindowDefinitionExpr'.

@since 1.1.0.0
-}
window :: Expr.NamedWindowDefinitionExpr -> SelectOptions
window namedWindow =
  emptySelectOptions
    { i_windowExpr = Just namedWindow
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.FetchClause'.

@since 1.1.0.0
-}
fetchRow :: Expr.FetchClause -> SelectOptions
fetchRow fetchClause =
  emptySelectOptions
    { i_fetchClause = First $ Just fetchClause
    }

{- |
  Builds a 'Expr.QueryExpr' that will use the specified 'Expr.SelectList' when
  building the @SELECT@ statement to execute. It is up to the caller to make
  sure that the 'Expr.SelectList' expression makes sense for the table being
  queried, and that the names of the columns in the result set match those
  expected by the 'Orville.PostgreSQL.SqlMarshaller' that is ultimately used to
  decode it.

  This function is useful for building more advanced queries that need to
  select things other than simple columns from the table, such as using
  aggregate functions. The 'Expr.SelectList' can be built however the caller
  desires. If Orville does not support building the 'Expr.SelectList' you need
  using any of the expression-building functions, you can resort to
  @RawSql.fromRawSql@ as an escape hatch to build the 'Expr.SelectList' here.

@since 1.0.0.0
-}
selectOptionsQueryExpr ::
  Expr.SelectList ->
  Expr.FromItemExpr ->
  SelectOptions ->
  Expr.QueryExpr
selectOptionsQueryExpr selectList tableReferenceList selectOptions =
  Expr.queryExpr
    (selectDistinct selectOptions)
    selectList
    ( Just $
        Expr.tableExpr
          tableReferenceList
          (selectWhereClause selectOptions)
          (selectGroupByClause selectOptions)
          (selectOrderByClause selectOptions)
          (selectLimitExpr selectOptions)
          (selectOffsetExpr selectOptions)
          (selectRowLockingClause selectOptions)
          (selectWindowClause selectOptions)
          (selectFetchClause selectOptions)
    )
