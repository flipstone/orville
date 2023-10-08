{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Internal.RowCountExpectation
  ( expectExactlyOneRow
  , expectAtMostOneRow
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))

{- |
  INTERNAL: This should really never get thrown in the real world. It would be
  thrown if the returning clause from an insert statement for a single record
  returned 0 records or more than 1 record.

@since 0.10.0.0
-}
newtype RowCountExpectationError
  = RowCountExpectationError String
  deriving (Show)

instance Exception RowCountExpectationError

expectExactlyOneRow :: MonadIO m => String -> [a] -> m a
expectExactlyOneRow caller rows =
  case rows of
    [row] ->
      pure row
    _ ->
      liftIO . throwIO . RowCountExpectationError $
        caller
          <> ": Expected exactly one row to be returned, but got "
          <> show (length rows)

expectAtMostOneRow :: MonadIO m => String -> [a] -> m (Maybe a)
expectAtMostOneRow caller rows =
  case rows of
    [] ->
      pure Nothing
    [row] ->
      pure (Just row)
    _ ->
      liftIO . throwIO . RowCountExpectationError $
        caller
          <> ": Expected exactly one row to be returned, but got "
          <> show (length rows)
