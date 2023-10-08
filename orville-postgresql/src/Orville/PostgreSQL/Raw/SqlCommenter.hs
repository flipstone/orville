{- |

Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

This module provides the very basics for [sqlcommenter](https://google.github.io/sqlcommenter)
support.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Raw.SqlCommenter
  ( SqlCommenterAttributes
  , addSqlCommenterAttributes
  )
where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Network.URI as URI

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | The representation of 'T.Text' key/value pairs for supporting the sqlcommenter specification.
  This allows you to attach key/values of 'T.Text' that supporting systems can use for advanced
  metrics. See [sqlcommenter](https://google.github.io/sqlcommenter) for details of the
  specification.

@since 1.0.0.0
-}
type SqlCommenterAttributes = Map.Map T.Text T.Text

{- | Adds a given 'SqlCommenter' set of key/value 'T.Text' pairs to a 'RawSql.SqlExpression'. This
  performs all of the required serialization for the given values. Note that no values are
  automatically added here, so any that you may wish to add can be freely set without a name clash
  of any kind from this function itself.

@since 1.0.0.0
-}
addSqlCommenterAttributes :: RawSql.SqlExpression a => SqlCommenterAttributes -> a -> a
addSqlCommenterAttributes commenter a =
  RawSql.unsafeFromRawSql $
    RawSql.toRawSql a
      <> keyValueSerializationToRawSql commenter

keyValueSerializationToRawSql :: SqlCommenterAttributes -> RawSql.RawSql
keyValueSerializationToRawSql =
  RawSql.fromText . keyValueSerialization

{- | Perform the sqlcommenter serialization on for the whole 'SqlCommenter' map of key/value pairs.
     The spec can be found
     [here](https://google.github.io/sqlcommenter/spec/#key-value-serialization)

@since 1.0.0.0
-}
keyValueSerialization :: SqlCommenterAttributes -> T.Text
keyValueSerialization =
  wrapInSqlComment . addCommasAndConcat . List.sort . fmap concatWithEquals . Map.toList . valueSerialization . keySerialization

addCommasAndConcat :: [T.Text] -> T.Text
addCommasAndConcat [] = T.pack "''"
addCommasAndConcat txts = T.concat $ List.intersperse (T.pack ",") txts

concatWithEquals :: (T.Text, T.Text) -> T.Text
concatWithEquals (k, v) =
  k <> T.pack "=" <> v

-- | The spec can be found [here](https://google.github.io/sqlcommenter/spec/#key-serialization)
keySerialization :: SqlCommenterAttributes -> SqlCommenterAttributes
keySerialization =
  Map.mapKeys escapeText

-- | The spec can be found [here](https://google.github.io/sqlcommenter/spec/#value-serialization)
valueSerialization :: SqlCommenterAttributes -> SqlCommenterAttributes
valueSerialization =
  fmap (wrapInSingleQuote . escapeQuote . escapeText)

-- Here we ensure there is a space before the comment
wrapInSqlComment :: T.Text -> T.Text
wrapInSqlComment txt =
  T.pack " /*" <> txt <> T.pack "*/"

wrapInSingleQuote :: T.Text -> T.Text
wrapInSingleQuote txt =
  T.pack "'" <> txt <> T.pack "'"

escapeQuote :: T.Text -> T.Text
escapeQuote =
  T.replace (T.pack "'") (T.pack "\'")

escapeText :: T.Text -> T.Text
escapeText =
  T.pack . escapeStr . T.unpack

escapeStr :: String -> String
escapeStr = URI.escapeURIString URI.isUnescapedInURIComponent
