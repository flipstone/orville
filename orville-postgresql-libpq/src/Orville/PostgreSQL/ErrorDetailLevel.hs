{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable
-}
module Orville.PostgreSQL.ErrorDetailLevel
  ( ErrorDetailLevel (ErrorDetailLevel, includeErrorMessage, includeSchemaNames, includeRowIdentifierValues, includeNonIdentifierValues),
    defaultErrorDetailLevel,
    minimalErrorDetailLevel,
    maximalErrorDetailLevel,
    redactErrorMessage,
    redactSchemaName,
    redactIdentifierValue,
    redactNonIdentifierValue,
  )
where

{- |
  'ErrorDetailLevel' provides a means to configure what elements of information
  are included in error messages that originate from decoding rows queried
  from the database. This can be specified either my manually rendering the
  error message and providing the desired configuration, or by setting the
  desired detail level in the @OrvilleState@ as a default.

  Information will be redacted from error messages for any of the fields
  that are set to @False@.
-}
data ErrorDetailLevel = ErrorDetailLevel
  { includeErrorMessage :: Bool
  , includeSchemaNames :: Bool
  , includeRowIdentifierValues :: Bool
  , includeNonIdentifierValues :: Bool
  }
  deriving (Show)

{- |
  A minimal 'ErrorDetailLevel' where everything all information (including
  any situationally-specific error message!) is redacted from error messages.
-}
minimalErrorDetailLevel :: ErrorDetailLevel
minimalErrorDetailLevel =
  ErrorDetailLevel
    { includeErrorMessage = False
    , includeSchemaNames = False
    , includeRowIdentifierValues = False
    , includeNonIdentifierValues = False
    }

{- |
  A default 'ErrorDetailLevel' that strikes balance of including all "Generic"
  information such as the error message, schema names and row identifiers, but
  avoids untentionally leaking non-identifier values from the database by
  redacting them.
-}
defaultErrorDetailLevel :: ErrorDetailLevel
defaultErrorDetailLevel =
  ErrorDetailLevel
    { includeErrorMessage = True
    , includeSchemaNames = True
    , includeRowIdentifierValues = True
    , includeNonIdentifierValues = False
    }

{- |
  A maximal 'ErrorDetailLevel' that redacts no information from the error
  messages. Error messages will include values from the database for any
  columns are involved in a decoding failure, including some which you may
  not have intended to expose through error message. Use with caution.
-}
maximalErrorDetailLevel :: ErrorDetailLevel
maximalErrorDetailLevel =
  ErrorDetailLevel
    { includeErrorMessage = True
    , includeSchemaNames = True
    , includeRowIdentifierValues = True
    , includeNonIdentifierValues = True
    }

{- |
  Redacts given the error message string if the 'ErrorDetailLevel' indicates
  that error messages should be redacted.
-}
redactErrorMessage :: ErrorDetailLevel -> String -> String
redactErrorMessage detailLevel message =
  if includeErrorMessage detailLevel
    then message
    else redactedValue

{- |
  Redacts given the schema name string if the 'ErrorDetailLevel' indicates
  that schema names should be redacted.
-}
redactSchemaName :: ErrorDetailLevel -> String -> String
redactSchemaName detailLevel schemaName =
  if includeSchemaNames detailLevel
    then schemaName
    else redactedValue

{- |
  Redacts given the identifier value string if the 'ErrorDetailLevel' indicates
  that identifier values should be redacted.
-}
redactIdentifierValue :: ErrorDetailLevel -> String -> String
redactIdentifierValue detailLevel idValue =
  if includeRowIdentifierValues detailLevel
    then idValue
    else redactedValue

{- |
  Redacts given the non-identifier value string if the 'ErrorDetailLevel' indicates
  that non-identifier values should be redacted.
-}
redactNonIdentifierValue :: ErrorDetailLevel -> String -> String
redactNonIdentifierValue detailLevel nonIdValue =
  if includeNonIdentifierValues detailLevel
    then nonIdValue
    else redactedValue

redactedValue :: String
redactedValue =
  "[REDACTED]"
