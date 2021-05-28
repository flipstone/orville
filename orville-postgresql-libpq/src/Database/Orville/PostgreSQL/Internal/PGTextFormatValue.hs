module Database.Orville.PostgreSQL.Internal.PGTextFormatValue
  ( PGTextFormatValue
  , NULByteFoundError(NULByteFoundError)
  , unsafeFromByteString
  , fromByteString
  , toByteString
  , toBytesForLibPQ
  ) where

import           Control.Exception (Exception)
import qualified Data.ByteString as BS

{-|
  A 'PGTextFormatValue' represents raw bytes that will be passed to postgresql
  via libpq. These bytes must conform to the TEXT format of values that
  postgresql expects. In all cases postgresql will be allowed to infer the type
  of the value based on its usage in the query.

  Note that postgresql does not allow NUL bytes in text values, and the LibPQ C
  library expects text values to be given as NULL-terminated C Strings, so
  '\NUL' bytes cannot be included in a 'PGTextFormatValue'. If 'fromByteString'
  is used to construct the 'PGTextFormatValue' (normally what you should do),
  an error will be raised before libpq is called to execute the query. If
  'unsafeFromByteString' is used, the caller is expected to ensure that no
  '\NUL' bytes are present. If a '\NUL' byte is included with
  'unsafeFromByteString', the value passed to the database will be truncated at
  the '\NUL' byte because it will be interpreted as the end of the C String by
  libpq.
-}
data PGTextFormatValue
  = NoAssumptionsMade BS.ByteString
  | AssumedToHaveNoNULValues BS.ByteString
  deriving (Show)

instance Eq PGTextFormatValue where
  left == right =
    toBytesForLibPQ left == toBytesForLibPQ right

data NULByteFoundError =
  NULByteFoundError
  deriving (Show, Eq)

instance Exception NULByteFoundError where

{-|
  Constructs a 'PGTextFormatValue' from the given bytes directly, without checking
  whether any of the bytes are '\NUL' or not. If a 'BS.ByteString' containing
  a '\NUL' byte is given, the value will be truncated at the '\NUL' when it
  is passed to libpq.

  This function is only safe to use when you have generated the bytestring
  in a way that guarantees no '\NUL' bytes are present, such as when serializing
  an integer value to its decimal representation.
-}
unsafeFromByteString :: BS.ByteString -> PGTextFormatValue
unsafeFromByteString =
  AssumedToHaveNoNULValues

{-|
  Constructs a 'PGTextFormatValue' from the given bytes, which will be checked
  to ensure none of them are '\NUL' before being passed to libpq. If a '\NUL'
  byte is found an error will be raised.
-}
fromByteString :: BS.ByteString -> PGTextFormatValue
fromByteString =
  NoAssumptionsMade

{-|
  Converts the 'PGTextFormatValue' to bytes intended to be passed to libpq.
  If any '\NUL' bytes are found, 'NULByteErrorFound' will be returned (unless
  'unsafeFromByteString' was used to construct the value).
-}
toBytesForLibPQ :: PGTextFormatValue -> Either NULByteFoundError BS.ByteString
toBytesForLibPQ value =
  case value of
    AssumedToHaveNoNULValues noNULBytes ->
      Right noNULBytes

    NoAssumptionsMade anyBytes ->
      if
        BS.elem 0 anyBytes
      then
        Left NULByteFoundError
      else
        Right anyBytes

{-|
  Convents the 'PGTextFormatValue' back to the bytes that were used to
  construct it, losing the information about whether it would be checked
  for '\NUL' bytes or not.
-}
toByteString :: PGTextFormatValue -> BS.ByteString
toByteString value =
  case value of
    AssumedToHaveNoNULValues bytes ->
      bytes

    NoAssumptionsMade bytes ->
      bytes
