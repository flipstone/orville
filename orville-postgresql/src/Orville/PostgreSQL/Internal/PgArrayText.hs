{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

Helpers for parsing and rendering the text representation PostgreSQL uses
for array values (e.g. @{a,b,c}@).

@since 1.2.0.0
-}
module Orville.PostgreSQL.Internal.PgArrayText
  ( pgArrayTextToList
  , pgArrayTextToTextList
  , textListToPgArrayText
  ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Text as T

{- | Parses the text representation of a PostgreSQL array (@{elem,elem,...}@)
  into its elements using the given element parser. The description is used
  in the error message when parsing fails.
-}
pgArrayTextToList :: String -> AttoText.Parser a -> T.Text -> Either String [a]
pgArrayTextToList description elementParser text =
  let
    parser = do
      _ <- AttoText.char '{'
      elements <- AttoText.sepBy elementParser (AttoText.char ',')
      _ <- AttoText.char '}'
      AttoText.endOfInput
      pure elements
  in
    case AttoText.parseOnly parser text of
      Left err -> Left ("Unable to decode PostgreSQL array as " <> description <> ": " <> err)
      Right elements -> Right elements

{- | Parses the text representation of a PostgreSQL array of textual values
  (such as a @name[]@ or @text[]@ column) into its elements. PostgreSQL
  renders elements containing special characters (commas, braces, quotes,
  backslashes, whitespace) double-quoted, using backslash escapes for quotes
  and backslashes within them.
-}
pgArrayTextToTextList :: String -> T.Text -> Either String [T.Text]
pgArrayTextToTextList description =
  let
    quotedChunk =
      AttoText.takeWhile1 (\c -> c /= '"' && c /= '\\')
        <|> (AttoText.char '\\' *> (T.singleton <$> AttoText.anyChar))

    quotedElement = do
      _ <- AttoText.char '"'
      chunks <- AttoText.many' quotedChunk
      _ <- AttoText.char '"'
      pure (T.concat chunks)

    unquotedElement =
      AttoText.takeWhile1 (\c -> c /= ',' && c /= '{' && c /= '}' && c /= '"' && c /= '\\')
  in
    pgArrayTextToList description (quotedElement <|> unquotedElement)

{- | Renders a list of textual values in PostgreSQL's array text syntax. Every
  element is rendered double-quoted, which is valid regardless of its
  content, with quotes and backslashes escaped. This is the inverse of
  'pgArrayTextToTextList'.
-}
textListToPgArrayText :: [T.Text] -> T.Text
textListToPgArrayText values =
  let
    escapeChar c =
      case c of
        '"' -> T.pack "\\\""
        '\\' -> T.pack "\\\\"
        _ -> T.singleton c

    quoteValue value =
      T.pack "\"" <> T.concatMap escapeChar value <> T.pack "\""
  in
    T.pack "{" <> T.intercalate (T.pack ",") (fmap quoteValue values) <> T.pack "}"
