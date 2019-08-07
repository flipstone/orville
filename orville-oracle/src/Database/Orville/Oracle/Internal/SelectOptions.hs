{-|
Module    : Database.Orville.Oracle.Internal.SelectOptions
Copyright : Flipstone Technology Partners 2016-2018
License   : MIT
-}
{-# LANGUAGE CPP#-}
{-# LANGUAGE RecordWildCards #-}

module Database.Orville.Oracle.Internal.SelectOptions where

import Data.Convertible
import qualified Data.List as List
import Data.Maybe
import Data.Monoid
import Database.HDBC

import Database.Orville.Oracle.Internal.FieldDefinition ()
import Database.Orville.Oracle.Internal.GroupBy
import Database.Orville.Oracle.Internal.OrderBy
import Database.Orville.Oracle.Internal.QueryKey
import Database.Orville.Oracle.Internal.Types ()
import Database.Orville.Oracle.Internal.Where

data SelectOptions = SelectOptions
  { selectDistinct :: First Bool
  , selectOptWhere :: [WhereCondition]
  , selectOptOrder :: [OrderByClause]
  , selectOptLimit :: First Int
  , selectOptOffset :: First Int
  , selectOptGroup :: [GroupByClause]
  }

selectOptLimitSql :: SelectOptions -> Maybe SqlValue
selectOptLimitSql = fmap convert . getFirst . selectOptLimit

selectOptOffsetSql :: SelectOptions -> Maybe SqlValue
selectOptOffsetSql = fmap convert . getFirst . selectOptOffset

#if MIN_VERSION_base(4,11,0)
instance Semigroup SelectOptions where
  (<>) = appendSelectOptions
#endif

instance Monoid SelectOptions where
  mempty = SelectOptions mempty mempty mempty mempty mempty mempty
  mappend = appendSelectOptions

appendSelectOptions :: SelectOptions -> SelectOptions -> SelectOptions
appendSelectOptions opt opt' =
  SelectOptions
    (selectDistinct  opt <> selectDistinct  opt')
    (selectOptWhere  opt <> selectOptWhere  opt')
    (selectOptOrder  opt <> selectOptOrder  opt')
    (selectOptLimit  opt <> selectOptLimit  opt')
    (selectOptOffset opt <> selectOptOffset opt')
    (selectOptGroup  opt <> selectOptGroup  opt')

instance QueryKeyable SelectOptions where
  queryKey opt =
    mconcat
      [ qkOp "WHERE" $ selectOptWhere opt
      , qkOp "GROUP" $ selectOptGroup opt
      , qkOp "ORDER" $ selectOptOrder opt
      , qkOp "LIMIT" $ selectOptLimitSql opt
      , qkOp "OFFSET" $ selectOptOffsetSql opt
      ]

selectClause :: SelectOptions -> String
selectClause opts =
  case selectDistinct opts of
    First (Just True)  -> "SELECT DISTINCT "
    _ -> "SELECT "

selectOptClause :: SelectOptions -> String
selectOptClause opts =
  List.intercalate
    " "
    [ selectWhereClause opts
    , selectGroupByClause opts
    , selectOrderByClause opts
    , selectLimitClause opts
    , selectOffsetClause opts
    ]

selectWhereClause :: SelectOptions -> String
selectWhereClause = whereClause . selectOptWhere

selectOrderByClause :: SelectOptions -> String
selectOrderByClause = clause . selectOptOrder
  where
    clause [] = ""
    clause sortClauses =
      "ORDER BY " ++ List.intercalate ", " (map sortingSql sortClauses)

selectGroupByClause :: SelectOptions -> String
selectGroupByClause = clause . selectOptGroup
  where
    clause [] = ""
    clause groupClauses =
      "GROUP BY " ++ List.intercalate ", " (map groupingSql groupClauses)

selectOptValues :: SelectOptions -> [SqlValue]
selectOptValues opts =
  concat
    [ whereValues $ selectOptWhere opts
    , concatMap groupingValues $ selectOptGroup opts
    , concatMap sortingValues $ selectOptOrder opts
    , maybeToList $ selectOptLimitSql opts
    , maybeToList $ selectOptOffsetSql opts
    ]

selectLimitClause :: SelectOptions -> String
selectLimitClause opts =
  case getFirst $ selectOptLimit opts of
    Nothing -> ""
    Just _ -> "LIMIT ?"

selectOffsetClause :: SelectOptions -> String
selectOffsetClause opts =
  case getFirst $ selectOptOffset opts of
    Nothing -> ""
    Just _ -> "OFFSET ?"

distinct :: SelectOptions
distinct = SelectOptions (First $ Just True) mempty mempty mempty mempty mempty

where_ :: WhereCondition -> SelectOptions
where_ clause = SelectOptions mempty [clause] mempty mempty mempty mempty

order :: ToOrderBy a => a -> SortDirection -> SelectOptions
order orderable dir =
  SelectOptions mempty mempty [toOrderBy orderable dir] mempty mempty mempty

limit :: Int -> SelectOptions
limit n = SelectOptions mempty mempty mempty (First $ Just n) mempty mempty

offset :: Int -> SelectOptions
offset n = SelectOptions mempty mempty mempty mempty (First $ Just n) mempty

groupBy :: ToGroupBy a => a -> SelectOptions
groupBy groupable =
  SelectOptions mempty mempty mempty mempty mempty [toGroupBy groupable]
