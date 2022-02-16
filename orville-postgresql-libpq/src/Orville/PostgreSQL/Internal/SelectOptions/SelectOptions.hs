module Orville.PostgreSQL.Internal.SelectOptions.SelectOptions
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

import qualified Data.List.NonEmpty as NEL
import Data.Monoid (First (First, getFirst))

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import qualified Orville.PostgreSQL.Internal.Extra.NonEmpty as ExtraNonEmpty
import Orville.PostgreSQL.Internal.SelectOptions.OrderBy (OrderBy, orderByToClause)
import Orville.PostgreSQL.Internal.SelectOptions.WhereCondition (WhereCondition, whereAnd, whereConditionToBooleanExpr)

{- |
   A 'SelectOptions' is a set of options that can be used to change the way
   a basic query function works by adding 'WHERE', 'ORDER BY', 'GROUP BY', etc.
   Functions are provided to construct 'SelectOptions' for individual options,
   which may then be combined via '<>' (also exposed as 'appendSelectOptions').
-}
data SelectOptions = SelectOptions
  { i_distinct :: First Bool
  , i_whereConditions :: [WhereCondition]
  , i_orderBy :: Maybe OrderBy
  , i_limitExpr :: First Expr.LimitExpr
  , i_offsetExpr :: First Expr.OffsetExpr
  , i_groupByExprs :: [Expr.GroupByExpr]
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
    , i_whereConditions = []
    , i_orderBy = mempty
    , i_limitExpr = mempty
    , i_offsetExpr = mempty
    , i_groupByExprs = []
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
    (i_whereConditions left <> i_whereConditions right)
    (i_orderBy left <> i_orderBy right)
    (i_limitExpr left <> i_limitExpr right)
    (i_offsetExpr left <> i_offsetExpr right)
    (i_groupByExprs left <> i_groupByExprs right)

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
  fmap (Expr.whereClause . whereConditionToBooleanExpr . ExtraNonEmpty.foldl1' whereAnd)
    . NEL.nonEmpty
    . i_whereConditions

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
  fmap orderByToClause . i_orderBy

{- |
  Builds the 'Expr.GroupByClause' that should be used to include the
  'GroupByClause's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'GroupByClause's have been specified.
-}
selectGroupByClause :: SelectOptions -> Maybe Expr.GroupByClause
selectGroupByClause =
  fmap (Expr.groupByClause . ExtraNonEmpty.foldl1' Expr.appendGroupByExpr)
    . NEL.nonEmpty
    . i_groupByExprs

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
where_ :: WhereCondition -> SelectOptions
where_ condition =
  emptySelectOptions
    { i_whereConditions = [condition]
    }

{- |
  Constructs a 'SelectOptions' with just the given 'OrderBy'.
-}
orderBy :: OrderBy -> SelectOptions
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
    { i_groupByExprs = [groupByExpr]
    }
