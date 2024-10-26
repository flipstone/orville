{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Marshall.MarshallError
  ( MarshallError (MarshallError, marshallErrorDetailLevel, marshallErrorRowIdentifier, marshallErrorDetails)
  , renderMarshallError
  , MarshallErrorDetails (DecodingError, MissingColumnError)
  , renderMarshallErrorDetails
  , DecodingErrorDetails (DecodingErrorDetails, decodingErrorValues, decodingErrorMessage)
  , renderDecodingErrorDetails
  , MissingColumnErrorDetails (MissingColumnErrorDetails, missingColumnName, actualColumnNames)
  , renderMissingColumnErrorDetails
  )
where

import Control.Exception (Exception)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

import Orville.PostgreSQL.ErrorDetailLevel (ErrorDetailLevel, redactErrorMessage, redactIdentifierValue, redactNonIdentifierValue, redactSchemaName)
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

{- | A 'MarshallError' may be returned from
  'Orville.PostgreSQL.Marshall.marshallResultFromSql' when a row being decoded
  from the database doesn't meet the expectations of the
  'Orville.PostgreSQL.Marshall.SqlMarshaller' that is decoding it.

@since 1.0.0.0
-}
data MarshallError = MarshallError
  { marshallErrorDetailLevel :: ErrorDetailLevel
  -- ^ The level of detail that will be used to render this error as a
  -- message if 'show' is called.
  , marshallErrorRowIdentifier :: [(B8.ByteString, SqlValue.SqlValue)]
  -- ^ The identifier of the row that caused the error. This is a list
  -- of pairs of column name and value in their raw form from the database
  -- to avoid further possible decoding errors when reading the values.
  , marshallErrorDetails :: MarshallErrorDetails
  -- ^ The detailed information about the error that occurred during
  -- decoding.
  }

instance Show MarshallError where
  show decodingError =
    renderMarshallError
      (marshallErrorDetailLevel decodingError)
      decodingError

instance Exception MarshallError

{- | Renders a 'MarshallError' to a string using the specified 'ErrorDetailLevel'.

  This ignores any 'ErrorDetailLevel' that was captured by default from
  the Orville context and uses the specified level of detail instead.

  You may want to use this function to render certain errors with a higher
  level of detail than you consider safe for (for example) your application
  logs while using a lower default error detail level with the 'Show' instance
  of 'MarshallError' in case an exception is handled in a more visible section
  of code that returns information more publicly (e.g. a request handler for a
  public endpoint).

@since 1.0.0.0
-}
renderMarshallError :: ErrorDetailLevel -> MarshallError -> String
renderMarshallError detailLevel marshallError =
  let
    presentableRowId =
      map
        (presentSqlColumnValue detailLevel redactIdentifierValue)
        (marshallErrorRowIdentifier marshallError)
  in
    concat
      [ "Unable to decode row with identifier ["
      , List.intercalate ", " presentableRowId
      , "]: "
      , renderMarshallErrorDetails detailLevel (marshallErrorDetails marshallError)
      ]

{- | A internal helper to present a redacted column name and sql value in an error
  message. The redacter function is passed as an argument here so that this
  function can be used to present either ID values or general values as
  required by the context of the caller.

@since 1.0.0.0
-}
presentSqlColumnValue ::
  ErrorDetailLevel ->
  (ErrorDetailLevel -> String -> String) ->
  (B8.ByteString, SqlValue.SqlValue) ->
  String
presentSqlColumnValue detailLevel redacter (columnName, sqlValue) =
  let
    sqlValueString =
      redacter detailLevel $
        SqlValue.foldSqlValue
          (B8.unpack . PgTextFormatValue.toByteString)
          (\vals -> "(" <> List.intercalate ", " (NE.toList vals) <> ")")
          "NULL"
          sqlValue
  in
    redactSchemaName detailLevel (B8.unpack columnName)
      <> " = "
      <> sqlValueString

{- | A 'MarshallErrorDetails' may be returned from
  'Orville.PostgreSQL.Marshall.marshallResultFromSql' if the result set being
  decoded from the database doesn't meet the expectations of the
  'Orville.PostgreSQL.Marshall.SqlMarshaller' that is decoding it.

@since 1.0.0.0
-}
data MarshallErrorDetails
  = -- | Indicates that one or more values in a column could not be decoded,
    -- either individually or as a group.
    DecodingError DecodingErrorDetails
  | -- | Indicates that an expected column was not found in the result set.
    MissingColumnError MissingColumnErrorDetails

{- | Renders a 'MarshallErrorDetails' to a 'String' with a specified
  'ErrorDetailLevel'.

@since 1.0.0.0
-}
renderMarshallErrorDetails :: ErrorDetailLevel -> MarshallErrorDetails -> String
renderMarshallErrorDetails detailLevel err =
  case err of
    DecodingError details -> renderDecodingErrorDetails detailLevel details
    MissingColumnError details -> renderMissingColumnErrorDetails detailLevel details

{- | Details about an error that occurred while decoding values found in a SQL
  result set.

@since 1.0.0.0
-}
data DecodingErrorDetails = DecodingErrorDetails
  { decodingErrorValues :: [(B8.ByteString, SqlValue.SqlValue)]
  , decodingErrorMessage :: String
  }

{- | Renders a 'DecodingErrorDetails' to a 'String' with a specified
  'ErrorDetailLevel'.

@since 1.0.0.0
-}
renderDecodingErrorDetails :: ErrorDetailLevel -> DecodingErrorDetails -> String
renderDecodingErrorDetails detailLevel details =
  let
    presentableErrorValues =
      map
        (presentSqlColumnValue detailLevel redactNonIdentifierValue)
        (decodingErrorValues details)
  in
    concat
      [ "Unable to decode columns from result set: "
      , redactErrorMessage detailLevel (decodingErrorMessage details)
      , ". Value(s) that failed to decode: ["
      , List.intercalate ", " presentableErrorValues
      , "]"
      ]

{- | Details about a column that was found to be missing in a SQL result set
  during decoding.

@since 1.0.0.0
-}
data MissingColumnErrorDetails = MissingColumnErrorDetails
  { missingColumnName :: B8.ByteString
  , actualColumnNames :: (Set.Set B8.ByteString)
  }

{- | Renders a 'MissingColumnErrorDetails' to a 'String' with a specified
  'ErrorDetailLevel'.

@since 1.0.0.0
-}
renderMissingColumnErrorDetails :: ErrorDetailLevel -> MissingColumnErrorDetails -> String
renderMissingColumnErrorDetails detailLevel details =
  let
    presentableActualNames =
      map
        (redactSchemaName detailLevel . B8.unpack)
        (Set.toList $ actualColumnNames details)
  in
    concat
      [ "Column "
      , redactSchemaName detailLevel (B8.unpack $ missingColumnName details)
      , " not found in results set. Actual columns were ["
      , List.intercalate ", " presentableActualNames
      , "]"
      ]
