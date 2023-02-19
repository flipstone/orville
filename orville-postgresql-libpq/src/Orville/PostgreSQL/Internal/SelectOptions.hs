module Orville.PostgreSQL.Internal.SelectOptions
  ( SelectOptions,
    emptySelectOptions,
    appendSelectOptions,
    selectDistinct,
    selectWhereClause,
    selectOrderByClause,
    selectGroupByClause,
    selectLimitExpr,
    selectOffsetExpr,
    distinct,
    where_,
    orderBy,
    limit,
    offset,
    groupBy,
  )
where

import Data.Monoid (First (First, getFirst))

import qualified Orville.PostgreSQL.Internal.Expr as Expr

{- |
   A 'SelectOptions' is a set of options that can be used to change the way
   a basic query function works by adding 'WHERE', 'ORDER BY', 'GROUP BY', etc.
   Functions are provided to construct 'SelectOptions' for individual options,
   which may then be combined via '<>' (also exposed as 'appendSelectOptions').
-}
data SelectOptions = SelectOptions
  { i_distinct :: First Bool
  , i_whereCondition :: Maybe Expr.BooleanExpr
  , i_orderBy :: Maybe Expr.OrderByExpr
  , i_limitExpr :: First Expr.LimitExpr
  , i_offsetExpr :: First Expr.OffsetExpr
  , i_groupByExpr :: Maybe Expr.GroupByExpr
  }

instance Semigroup SelectOptions where
  (<>) = appendSelectOptions

instance Monoid SelectOptions where
  mempty = emptySelectOptions

{- |
  A set of empty 'SelectOptions' that will not change how a query is run.
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
-}
selectWhereClause :: SelectOptions -> Maybe Expr.WhereClause
selectWhereClause =
  fmap Expr.whereClause . i_whereCondition

{- |
  Constructs a 'SelectOptions' with just 'distinct' set to 'True'.
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
-}
selectOrderByClause :: SelectOptions -> Maybe Expr.OrderByClause
selectOrderByClause =
  fmap Expr.orderByClause . i_orderBy

{- |
  Builds the 'Expr.GroupByClause' that should be used to include the
  'GroupByClause's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'GroupByClause's have been specified.
-}
selectGroupByClause :: SelectOptions -> Maybe Expr.GroupByClause
selectGroupByClause =
  fmap Expr.groupByClause . i_groupByExpr

{- |
  Builds a 'Expr.LimitExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any)
-}
selectLimitExpr :: SelectOptions -> Maybe Expr.LimitExpr
selectLimitExpr =
  getFirst . i_limitExpr

{- |
  Builds a 'Expr.OffsetExpr' that will limit the query results to the
  number specified in the 'SelectOptions' (if any)
-}
selectOffsetExpr :: SelectOptions -> Maybe Expr.OffsetExpr
selectOffsetExpr =
  getFirst . i_offsetExpr

{- |
  Constructs a 'SelectOptions' with just the given 'WhereCondition'.
-}
where_ :: Expr.BooleanExpr -> SelectOptions
where_ condition =
  emptySelectOptions
    { i_whereCondition = Just condition
    }

{- |
  Constructs a 'SelectOptions' with just the given 'Expr.OrderByExpr'.
-}
orderBy :: Expr.OrderByExpr -> SelectOptions
orderBy order =
  emptySelectOptions
    { i_orderBy = Just order
    }

{- |
  Constructs a 'SelectOptions' that will apply the given limit
-}
limit :: Int -> SelectOptions
limit limitValue =
  emptySelectOptions
    { i_limitExpr = First . Just . Expr.limitExpr $ limitValue
    }

{- |
  Constructs a 'SelectOptions' that will apply the given offset
-}
offset :: Int -> SelectOptions
offset offsetValue =
  emptySelectOptions
    { i_offsetExpr = First . Just . Expr.offsetExpr $ offsetValue
    }

{- |
  Constructs a 'SelectOptions' with just the given 'GroupByClause'.
-}
groupBy :: Expr.GroupByExpr -> SelectOptions
groupBy groupByExpr =
  emptySelectOptions
    { i_groupByExpr = Just groupByExpr
    }
