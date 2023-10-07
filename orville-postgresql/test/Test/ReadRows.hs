module Test.ReadRows
  ( readRows
  )
where

import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Raw.SqlValue (SqlValue)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

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
  if n > 0
    then [0 .. (n - 1)]
    else []
