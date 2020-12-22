{-|
Module    : Database.Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.ExecutionResult
  ( decodeRows
  ) where

import qualified Database.PostgreSQL.LibPQ as LibPQ

import Database.Orville.PostgreSQL.Internal.SqlType (SqlType(sqlTypeFromSql))

-- N.B. This only works for the first column of a table currently.
-- If there are no results in the given `Result` then we return an empty list
-- Otherwise we attempt to decode each result with the given `SqlType`.
decodeRows :: LibPQ.Result -> SqlType a -> IO [Maybe a]
decodeRows res sqlType = do
  nrows <- LibPQ.ntuples res
  let
    rowList =
      if
        nrows > 0
      then
        [0..(nrows - 1)]
      else
        []
    -- N.B. the usage of `getvalue'` here is important as this version returns a
    -- _copy_ of the data in the `Result` rather than a _reference_.
    -- This allows the `Result` to be garbage collected instead of being held onto indefinitely.
    getValue row =
      LibPQ.getvalue' res row (LibPQ.toColumn (0::Int))
    applySqlConv maybeBS =
      case maybeBS of
        Just bs -> sqlTypeFromSql sqlType bs
        Nothing -> Nothing
  maybeBSs <- traverse getValue rowList
  pure $ fmap applySqlConv maybeBSs
