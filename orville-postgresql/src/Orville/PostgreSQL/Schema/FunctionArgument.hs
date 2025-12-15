{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2025
License   : MIT
Stability : Stable

@since 1.1.0.0.4
-}
module Orville.PostgreSQL.Schema.FunctionArgument
  ( FunctionArgument
  )
where

import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | An argument passed to a function E.G. the @x int@ in

> CREATE FUNCTION my_function(x int, y int) RETURNS text

Support for using this type in orville is currently very limited and experimental

@since 1.1.0.0.4
-}
newtype FunctionArgument = FunctionArgument RawSql.RawSql
  deriving
    ( -- \| @since 1.1.0.0.4
      RawSql.SqlExpression
    )
