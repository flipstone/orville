{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.PgTextFormatValue
  ( PgTextFormatValue
  , NULByteFoundError (NULByteFoundError)
  , unsafeFromByteString
  , fromByteString
  , toByteString
  , toBytesForLibPQ
  )
where

import Control.Exception (Exception)
import qualified Data.ByteString as BS

{- |
  A 'PgTextFormatValue' represents raw bytes that will be passed to PostgreSQL
  via LibPQ. These bytes must conform to the TEXT format of values that
  PostgreSQL expects. In all cases, PostgreSQL will be allowed to infer the
  type of the value based on its usage in the query.

  Note that PostgreSQL does not allow NUL bytes in text values, and the LibPQ C
  library expects text values to be given as NULL-terminated C Strings, so
  '\NUL' bytes cannot be included in a 'PgTextFormatValue'. If 'fromByteString'
  is used to construct the 'PgTextFormatValue' (normally what you should do),
  an error will be raised before LibPQ is called to execute the query. If
  'unsafeFromByteString' is used, the caller is expected to ensure that no
  '\NUL' bytes are present. If a '\NUL' byte is included with
  'unsafeFromByteString', the value passed to the database will be truncated at
  the '\NUL' byte because it will be interpreted as the end of the C String by
  LibPQ.

@since 1.0.0.0
-}
data PgTextFormatValue
  = NoAssumptionsMade BS.ByteString
  | AssumedToHaveNoNULValues BS.ByteString
  deriving
    ( -- | @since 1.0.0.0
      Show
    )

-- | @since 1.0.0.0
instance Eq PgTextFormatValue where
  left == right =
    toBytesForLibPQ left == toBytesForLibPQ right

data NULByteFoundError
  = NULByteFoundError
  deriving
    ( -- | @since 1.0.0.0
      Show
    , -- | @since 1.0.0.0
      Eq
    )

-- | @since 1.0.0.0
instance Exception NULByteFoundError

{- |
  Constructs a 'PgTextFormatValue' from the given bytes directly, without checking
  whether any of the bytes are '\NUL' or not. If a 'BS.ByteString' containing
  a '\NUL' byte is given, the value will be truncated at the '\NUL' when it
  is passed to LibPQ.

  This function is only safe to use when you have generated the bytestring
  in a way that guarantees no '\NUL' bytes are present, such as when serializing
  an integer value to its decimal representation.

@since 1.0.0.0
-}
unsafeFromByteString :: BS.ByteString -> PgTextFormatValue
unsafeFromByteString =
  AssumedToHaveNoNULValues

{- |
  Constructs a 'PgTextFormatValue' from the given bytes, which will be checked
  to ensure none of them are '\NUL' before being passed to LibPQ. If a '\NUL'
  byte is found an error will be raised.

@since 1.0.0.0
-}
fromByteString :: BS.ByteString -> PgTextFormatValue
fromByteString =
  NoAssumptionsMade

{- |
  Converts the 'PgTextFormatValue' to bytes intended to be passed to LibPQ.
  If any '\NUL' bytes are found, 'NULByteFoundError' will be returned (unless
  'unsafeFromByteString' was used to construct the value).

@since 1.0.0.0
-}
toBytesForLibPQ :: PgTextFormatValue -> Either NULByteFoundError BS.ByteString
toBytesForLibPQ value =
  case value of
    AssumedToHaveNoNULValues noNULBytes ->
      Right noNULBytes
    NoAssumptionsMade anyBytes ->
      if BS.elem 0 anyBytes
        then Left NULByteFoundError
        else Right anyBytes

{- |
  Converts the 'PgTextFormatValue' back to the bytes that were used to
  construct it, losing the information about whether it would be checked
  for '\NUL' bytes or not.

@since 1.0.0.0
-}
toByteString :: PgTextFormatValue -> BS.ByteString
toByteString value =
  case value of
    AssumedToHaveNoNULValues bytes ->
      bytes
    NoAssumptionsMade bytes ->
      bytes
