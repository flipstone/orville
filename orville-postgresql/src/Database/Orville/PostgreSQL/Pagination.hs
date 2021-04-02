module Database.Orville.PostgreSQL.Pagination
  ( Pagination(..)
  , buildPagination
  ) where

import Data.Monoid
import Data.Maybe (maybeToList)
import Safe (lastMay)

import Database.Orville.PostgreSQL.Internal.Monad (MonadOrville)
import Database.Orville.PostgreSQL.Internal.OrderBy (SortDirection(Ascending))
import Database.Orville.PostgreSQL.Internal.SelectOptions (limit, order, where_)
import Database.Orville.PostgreSQL.Internal.Types (TableDefinition(..), FieldDefinition, NotNull)
import Database.Orville.PostgreSQL.Internal.Where ((.>=), WhereCondition, whereAnd)
import Database.Orville.PostgreSQL.Select (runSelect, selectQueryTable)

data Pagination m entity =
  Pagination
    { pageRows :: [entity]
    , pageNext :: Maybe (m (Pagination m entity))
    }

buildPagination :: (MonadOrville conn m, Bounded orderField, Enum orderField)
                => TableDefinition readEnt write key
                -> FieldDefinition NotNull orderField
                -> (readEnt -> orderField)
                -> Maybe WhereCondition
                -> Word
                -> m (Pagination m readEnt)
buildPagination tableDef orderField getOrderField mbWhereCond pageSize =
  let selectOpts bound
        = order orderField Ascending
       <> limit (fromIntegral pageSize)
       <> where_ (whereAnd $ orderField .>= bound
                           : maybeToList mbWhereCond
                 )

      selectAll = runSelect . selectQueryTable tableDef
      mkPagination bound = do
        rows <- selectAll $ selectOpts bound

        let nxt =
              case lastMay rows of
                Just lst | length rows == fromIntegral pageSize ->
                  Just . mkPagination . succ $ getOrderField lst
                _ -> Nothing

        pure $ Pagination
                 { pageRows = rows
                 , pageNext = nxt
                 }

   in mkPagination minBound
