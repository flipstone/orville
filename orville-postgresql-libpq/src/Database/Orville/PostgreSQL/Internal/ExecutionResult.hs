{-|
Module    : Database.Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.ExecutionResult
  ( decodeRows
  , readRows
  ) where

import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.LibPQ as LibPQ

import           Database.Orville.PostgreSQL.Internal.SqlType (SqlType(sqlTypeFromSql))
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

readRows :: LibPQ.Result -> IO [[(Maybe BS.ByteString, SqlValue)]]
readRows res = do
  nrows <- LibPQ.ntuples res
  nfields <- LibPQ.nfields res

  let
    rowIndices =
      listOfIndicesByCount nrows

    fieldIndices =
      listOfIndicesByCount nfields

    -- N.B. the usage of `getvalue'` here is important as this version returns a
    -- _copy_ of the data in the `Result` rather than a _reference_.
    -- This allows the `Result` to be garbage collected instead of being held onto indefinitely.
    readValue rowIndex fieldIndex = do
      name <- LibPQ.fname res fieldIndex
      rawValue <- LibPQ.getvalue' res rowIndex fieldIndex
      pure $
        ( name
        , SqlValue.fromRawBytesNullable rawValue
        )

    readRow rowIndex =
      traverse (readValue rowIndex) fieldIndices

  traverse readRow rowIndices

listOfIndicesByCount :: (Num n, Ord n, Enum n) => n -> [n]
listOfIndicesByCount n =
  if
    n > 0
  then
    [0..(n - 1)]
  else
    []


-- N.B. This only works for the first column of a table currently.
-- If there are no results in the given `Result` then we return an empty list
-- Otherwise we attempt to decode each result with the given `SqlType`.
decodeRows :: LibPQ.Result -> SqlType a -> IO [Maybe a]
decodeRows res sqlType = do
  rows <- readRows res
  pure $ map (decodeSingleValue sqlType) rows

decodeSingleValue :: SqlType a -> [(key, SqlValue)] -> Maybe a
decodeSingleValue sqlType row =
  case row of
    [] ->
      Nothing

    (_, sqlValue) : _ ->
      sqlTypeFromSql sqlType sqlValue
