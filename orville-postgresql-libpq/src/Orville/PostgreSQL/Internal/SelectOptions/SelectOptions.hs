module Orville.PostgreSQL.Internal.SelectOptions.SelectOptions
  ( SelectOptions,
    emptySelectOptions,
    appendSelectOptions,
    selectWhereClause,
    where_,
    limit,
    offset,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid (First (First))

import qualified Orville.PostgreSQL.Internal.Expr as Expr
import Orville.PostgreSQL.Internal.SelectOptions.WhereCondition (WhereCondition, whereAnd, whereConditionToBooleanExpr)

{- |
   A 'SelectOptions' is a set of options that can be used to change the way
   a basic query function works by adding 'WHERE', 'ORDER BY', 'GROUP BY', etc.
   Functions are provided to construct 'SelectOptions' for individual options,
   which may then be combined via '<>' (also exposed as 'appendSelectOptions').
-}
data SelectOptions = SelectOptions
  { i_whereConditions :: [WhereCondition]
  , i_limitExpr :: First Expr.LimitExpr
  , i_offsetExpr :: First Expr.OffsetExpr
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
    { i_whereConditions = []
    , i_limitExpr = mempty
    , i_offsetExpr = mempty
    }

{- |
  Combines multple select options together, unioning the options together where
  possible. For options where this is not possible, (e.g. 'LIMIT'), the one
  on the left is preferred.
-}
appendSelectOptions :: SelectOptions -> SelectOptions -> SelectOptions
appendSelectOptions left right =
  SelectOptions
    (i_whereConditions left <> i_whereConditions right)
    (i_limitExpr left <> i_limitExpr right)
    (i_offsetExpr left <> i_offsetExpr right)

{- |
  Builds the 'Expr.WhereClause' that should be used to include the
  'WhereCondition's from the 'SelectOptions' on a query. This will be 'Nothing'
  where no 'WhereCondition's have been specified.
-}
selectWhereClause :: SelectOptions -> Maybe Expr.WhereClause
selectWhereClause selectOptions =
  case i_whereConditions selectOptions of
    [] ->
      Nothing
    (first : rest) ->
      Just . Expr.whereClause . whereConditionToBooleanExpr $ whereAnd (first :| rest)

{- |
  Constructs a 'SelectOptions' with just the given 'WhereCondition'.
-}
where_ :: WhereCondition -> SelectOptions
where_ condition =
  emptySelectOptions
    { i_whereConditions = [condition]
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
