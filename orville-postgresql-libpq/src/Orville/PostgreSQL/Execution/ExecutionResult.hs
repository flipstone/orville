{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021-2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Execution.ExecutionResult
  ( ExecutionResult (..),
    Column (..),
    Row (..),
    FakeLibPQResult,
    mkFakeLibPQResult,
    readRows,
  )
where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Database.PostgreSQL.LibPQ as LibPQ

import Orville.PostgreSQL.Raw.SqlValue (SqlValue)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- |
  A trivial wrapper for `Int` to help keep track of column vs row number

@since 0.10.0.0
-}
newtype Column
  = Column Int
  deriving
    ( -- | @since 0.10.0.0
      Eq
    , -- | @since 0.10.0.0
      Ord
    , -- | @since 0.10.0.0
      Enum
    )

{- |
  A trivial wrapper for `Int` to help keep track of column vs row number

@since 0.10.0.0
-}
newtype Row
  = Row Int
  deriving
    ( -- | @since 0.10.0.0
      Eq
    , -- | @since 0.10.0.0
      Ord
    , -- | @since 0.10.0.0
      Enum
    )

{- |
  `ExecutionResult` is a common interface for types that represent a result
  set returned from the database. For real, live database interactions this
  the concrete type will be a `LibPQ.Result`, but the `FakeLibPQResult`
  may be useful as well if you are writing custom code for decoding result
  sets and want to test aspects of the decoding that don't require a real
  database.

@since 0.10.0.0
-}
class ExecutionResult result where
  maxRowNumber :: result -> IO (Maybe Row)
  maxColumnNumber :: result -> IO (Maybe Column)
  columnName :: result -> Column -> IO (Maybe BS.ByteString)
  getValue :: result -> Row -> Column -> IO SqlValue

-- | @since 0.10.0.0
instance ExecutionResult LibPQ.Result where
  maxRowNumber result = do
    rowCount <- fmap fromEnum (LibPQ.ntuples result)
    pure $
      if rowCount > 0
        then Just $ Row (rowCount - 1)
        else Nothing

  maxColumnNumber result = do
    columnCount <- fmap fromEnum (LibPQ.nfields result)
    pure $
      if columnCount > 0
        then Just $ Column (columnCount - 1)
        else Nothing

  columnName result =
    LibPQ.fname result . LibPQ.toColumn . fromEnum

  getValue result (Row row) (Column column) =
    SqlValue.fromRawBytesNullable
      <$> LibPQ.getvalue' result (LibPQ.toRow row) (LibPQ.toColumn column)

{- |
  `FakeLibPQResult` provides a fake, in memory implementation of
  `ExecutionResult`.  This is mostly useful for writing automated tests that
  can assume a result set has been loaded and just need to test decoding the
  results.

@since 0.10.0.0
-}
data FakeLibPQResult = FakeLibPQResult
  { fakeLibPQColumns :: Map.Map Column BS.ByteString
  , fakeLibPQRows :: Map.Map Row (Map.Map Column SqlValue)
  }

{- |
  Constructs a `FakeLibPQResult`. The column names given as associated with
  the values for each row by their position in list. Any missing values (e.g.
  because a row is shorter than the heeader list) are treated as a SQL Null
  value.

@since 0.10.0.0
-}
mkFakeLibPQResult ::
  -- | The column names for the result set
  [BS.ByteString] ->
  -- | The row data for the result set
  [[SqlValue]] ->
  FakeLibPQResult
mkFakeLibPQResult columnList valuesList =
  let indexedRows = do
        (rowNumber, row) <- zip [Row 0 ..] valuesList

        let indexedColumns = zip [Column 0 ..] row

        pure (rowNumber, Map.fromList indexedColumns)
   in FakeLibPQResult
        { fakeLibPQColumns = Map.fromList (zip [Column 0 ..] columnList)
        , fakeLibPQRows = Map.fromList indexedRows
        }

fakeLibPQMaxRow :: FakeLibPQResult -> Maybe Row
fakeLibPQMaxRow =
  fmap fst . Map.lookupMax . fakeLibPQRows

fakeLibPQMaxColumn :: FakeLibPQResult -> Maybe Column
fakeLibPQMaxColumn result =
  let maxColumnsByRow =
        map fst
          . Maybe.mapMaybe Map.lookupMax
          . Map.elems
          . fakeLibPQRows
          $ result
   in case maxColumnsByRow of
        [] ->
          Nothing
        _ ->
          Just (maximum maxColumnsByRow)

-- | @since 0.10.0.0
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
  Maybe.fromMaybe SqlValue.sqlNull $ do
    row <- Map.lookup rowNumber (fakeLibPQRows result)
    Map.lookup columnNumber row

{- | 'readRows' will consume the 'LibPQ.Result', resulting in the collection of name for the field and
  'SqlValue' for each field in a row and for each row.

@since 0.10.0.0
-}
readRows :: LibPQ.Result -> IO [[(Maybe BS.ByteString, SqlValue)]]
readRows res = do
  nrows <- LibPQ.ntuples res
  nfields <- LibPQ.nfields res

  let rowIndices =
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
