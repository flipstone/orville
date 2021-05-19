{-|
Module    : Database.Orville.PostgreSQL.SqlType
Copyright : Flipstone Technology Partners 2016-2020
License   : MIT
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.Orville.PostgreSQL.Internal.ExecutionResult
  ( ExecutionResult(..)
  , Column(..)
  , Row(..)
  , FakeLibPQResult
  , mkFakeLibPQResult
  , decodeRows
  , readRows
  ) where

import qualified Data.ByteString as BS
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Database.PostgreSQL.LibPQ as LibPQ

import           Database.Orville.PostgreSQL.Internal.SqlType (SqlType(sqlTypeFromSql))
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)
import qualified Database.Orville.PostgreSQL.Internal.SqlValue as SqlValue

{-|
  A trivial wrapper for `Int` to help keep track of column vs row number
-}
newtype Column =
  Column Int
  deriving (Eq, Ord, Enum)

{-|
  A trivial wrapper for `Int` to help keep track of column vs row number
-}
newtype Row =
  Row Int
  deriving (Eq, Ord, Enum)

{-|
  `ExecutionResult` is a common interface for types that represent a result
  set returned from the database. For real, live database interactions this
  the concrete type will be a `LibPQ.Result`, but the `FakeLibPQResult`
  may be useful as well if you are writing custom code for decoding result
  sets and want to test aspects of the decoding that don't require a real
  database.
-}
class ExecutionResult result where
  maxRowNumber    :: result -> IO (Maybe Row)
  maxColumnNumber :: result -> IO (Maybe Column)
  columnName      :: result -> Column -> IO (Maybe BS.ByteString)
  getValue        :: result -> Row -> Column -> IO SqlValue

instance ExecutionResult LibPQ.Result where
  maxRowNumber result = do
    rowCount <- fmap fromEnum (LibPQ.ntuples result)
    pure $
      if
        rowCount > 0
      then
        Just $ Row (rowCount - 1)
      else
        Nothing

  maxColumnNumber result = do
    columnCount <- fmap fromEnum (LibPQ.nfields result)
    pure $
      if
        columnCount > 0
      then
        Just $ Column (columnCount - 1)
      else
        Nothing


  columnName result =
    LibPQ.fname result . LibPQ.toColumn . fromEnum

  getValue result (Row row) (Column column) =
    SqlValue.fromRawBytesNullable <$>
      LibPQ.getvalue' result (LibPQ.toRow row) (LibPQ.toColumn column)

{-|
  `FakeLibPQResult` provides a fake, in memory implementation of
  `ExecutionResult`.  This is mostly useful for writing automated tests that
  can assume a result set has been loaded and just need to test decoding the
  results.
-}
data FakeLibPQResult =
  FakeLibPQResult
    { fakeLibPQColumns     :: Map.Map Column BS.ByteString
    , fakeLibPQMaxColumn   :: Maybe Column
    , fakeLibPQMaxRow      :: Maybe Row
    , fakeLibPQValues      :: Map.Map (Row, Column) SqlValue
    }

{-|
  Constructs a `FakeLibPQResult`. The column names given as associated with
  the values for each row by their position in list. Any missing values (e.g.
  because a row is shorter than the heeader list) are treated as a SQL Null
  value.
-}
mkFakeLibPQResult :: [BS.ByteString] -- ^ The column names for the result set
                  -> [[SqlValue]] -- ^ The row data for the result set
                  -> FakeLibPQResult
mkFakeLibPQResult columnList valuesList =
  let
    indexedValues = do
      (rowNumber, row) <- zip [Row 0..] valuesList
      (columnNumber, value) <- zip [Column 0..] row
      pure ((rowNumber, columnNumber), value)

    maxIndex :: (Row, Column) -> (Row, Column) -> (Row, Column)
    maxIndex (rowA, columnA) (rowB, columnB) =
      (max rowA rowB, max columnA columnB)

    (maxRow, maxColumn) =
      case map fst indexedValues of
        [] ->
          (Nothing, Nothing)

        firstIndex : rest ->
          let
            (row, column) =
              foldl' maxIndex firstIndex rest
          in
            (Just row, Just column)

  in
    FakeLibPQResult
      { fakeLibPQColumns     = Map.fromList (zip [Column 0..] columnList)
      , fakeLibPQValues      = Map.fromList indexedValues
      , fakeLibPQMaxRow      = maxRow
      , fakeLibPQMaxColumn   = maxColumn
      }

instance ExecutionResult FakeLibPQResult where
  maxRowNumber = pure . fakeLibPQMaxRow
  maxColumnNumber = pure . fakeLibPQMaxColumn
  columnName result = pure . fakeLibPQColumnName result
  getValue result column = pure . fakeLibPQGetValue result column

fakeLibPQColumnName :: FakeLibPQResult -> Column -> (Maybe BS.ByteString)
fakeLibPQColumnName result column =
  Map.lookup column (fakeLibPQColumns result)

fakeLibPQGetValue :: FakeLibPQResult -> Row -> Column -> SqlValue
fakeLibPQGetValue result rowNumber columnNumber =
  Map.findWithDefault
    SqlValue.sqlNull
    (rowNumber, columnNumber)
    (fakeLibPQValues result)


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
