{-|
Module    : Data.String.Helpers
Copyright : Flipstone Technology Partners 2019
License   : MIT

Helpers for dealing with String/Text particularly escaping
-}
module Data.String.Helpers
  ( escapeString
  )
where

escapeString :: String -> String
escapeString str = "\"" <> str <> "\""
