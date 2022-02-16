{-# LANGUAGE GADTs #-}

module Orville.PostgreSQL.Internal.ReturningOption
  ( ReturningOption (..),
    ReturningClause,
    NoReturningClause,
  )
where

data ReturningClause
data NoReturningClause

{- |
  Specifies whether or not a @RETURNING@ clause should be included when a
  query expression is built. This type is found as a parameter on a number
  of the query building functions related to 'TableDefinition'.
-}
data ReturningOption clause where
  WithReturning :: ReturningOption ReturningClause
  WithoutReturning :: ReturningOption NoReturningClause
