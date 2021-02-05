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
import Database.Orville.PostgreSQL.Internal.Types (TableDefinition(..))
import Database.Orville.PostgreSQL.Internal.Where ((.>=), WhereCondition, whereAnd)
import Database.Orville.PostgreSQL.Select (runSelect, selectQueryTable)

data Pagination m entity =
  Pagination
    { pageRows :: [entity]
    , pageNext :: Maybe (m (Pagination m entity))
    }

buildPagination :: (MonadOrville conn m, Bounded key, Enum key)
                => TableDefinition readEnt write key
                -> Maybe WhereCondition
                -> Word
                -> m (Pagination m readEnt)
buildPagination tableDef mbWhereCond pageSize =
  let selectOpts bound
        = order (tablePrimaryKey tableDef) Ascending
       <> limit (fromIntegral pageSize)
       <> where_ (whereAnd $ tablePrimaryKey tableDef .>= bound
                           : maybeToList mbWhereCond
                 )

      selectAll = runSelect . selectQueryTable tableDef
      mkPagination bound = do
        rows <- selectAll $ selectOpts bound

        let nxt =
              case lastMay rows of
                Just lst | length rows == fromIntegral pageSize ->
                  Just . mkPagination . succ $ tableGetKey tableDef lst
                _ -> Nothing

        pure $ Pagination
                 { pageRows = rows
                 , pageNext = nxt
                 }

   in mkPagination minBound
