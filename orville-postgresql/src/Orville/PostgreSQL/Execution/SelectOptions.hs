{- |
Copyright : Flipstone Technology Partners 2023
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
  , distinct
  , where_
  , orderBy
  , limit
  , offset
  , groupBy
  , selectOptionsQueryExpr
  )
where

import Data.Monoid (First (First, getFirst))

import qualified Orville.PostgreSQL.Expr as Expr

{- |
   A 'SelectOptions' is a set of options that can be used to change the way
   a basic query function works by adding 'WHERE', 'ORDER BY', 'GROUP BY', etc.
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
    }

{- |
  Combines multple select options together, unioning the options together where
  possible. For options where this is not possible, (e.g. 'LIMIT'), the one
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
  'WhereCondition's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'WhereCondition's have been specified.

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
  'OrderByClause's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'OrderByClause's have been specified.

@since 1.0.0.0
-}
selectOrderByClause :: SelectOptions -> Maybe Expr.OrderByClause
selectOrderByClause =
  fmap Expr.orderByClause . i_orderBy

{- |
  Builds the 'Expr.GroupByClause' that should be used to include the
  'GroupByClause's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'GroupByClause's have been specified.

@since 1.0.0.0
-}
selectGroupByClause :: SelectOptions -> Maybe Expr.GroupByClause
selectGroupByClause =
  fmap Expr.groupByClause . i_groupByExpr

{- |
  Builds a 'Expr.LimitExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any)

@since 1.0.0.0
-}
selectLimitExpr :: SelectOptions -> Maybe Expr.LimitExpr
selectLimitExpr =
  getFirst . i_limitExpr

{- |
  Builds a 'Expr.OffsetExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any)

@since 1.0.0.0
-}
selectOffsetExpr :: SelectOptions -> Maybe Expr.OffsetExpr
selectOffsetExpr =
  getFirst . i_offsetExpr

{- |
  Constructs a 'SelectOptions' with just the given 'WhereCondition'.

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
  Constructs a 'SelectOptions' with just the given 'GroupByClause'.

@since 1.0.0.0
-}
groupBy :: Expr.GroupByExpr -> SelectOptions
groupBy groupByExpr =
  emptySelectOptions
    { i_groupByExpr = Just groupByExpr
    }

{- |
  Builds a 'QueryExpr' that will use the specified 'Expr.SelectList' when building
  the @SELECT@ statement to execute. It it up to the caller to make sure that
  the 'Expr.SelectList' expression makes sens for the table being queried, and
  that the names of the columns in the result set match those expected by the
  given 'SqlMarshaller', which will be used to decode it.

  This function is useful for building more advanced queries that need to
  select things other than simple columns from the table, such as using
  aggregate functions. The 'Expr.SelectList' can be built however the caller
  desires. If Orville does not support building the 'Expr.SelectList' you need
  using any of the expression building functions, you can resort to
  @RawSql.fromRawSql@ as an escape hatch to build the 'Expr.SelectList' here.

@since 1.0.0.0
-}
selectOptionsQueryExpr ::
  Expr.SelectList ->
  Expr.TableReferenceList ->
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
    )
