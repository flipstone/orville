{-# LANGUAGE GADTs #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 0.10.0.0
-}
module Orville.PostgreSQL.Execution.ReturningOption
  ( ReturningOption (..)
  , ReturningClause
  , NoReturningClause
  )
where

{- | A tag, used with 'ReturningOption' to indicate a SQL Returning clause.

@since 0.10.0.0
-}
data ReturningClause

{- | A tag, used with 'ReturningOption' to indicate no SQL Returning clause.

@since 0.10.0.0
-}
data NoReturningClause

{- |
  Specifies whether or not a @RETURNING@ clause should be included when a
  query expression is built. This type is found as a parameter on a number
  of the query building functions related to 'TableDefinition'.
@since 0.10.0.0
-}
data ReturningOption clause where
  WithReturning :: ReturningOption ReturningClause
  WithoutReturning :: ReturningOption NoReturningClause
