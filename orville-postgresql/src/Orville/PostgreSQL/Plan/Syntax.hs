{- ORMOLU_DISABLE -}
{-
  Disable formatting this comment so that fourmolu doesn't complain about
  qualified do in the example code.
-}
{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT

This module exports the 'Plan.bind' function as '>>=' so that it can be used in
conjuction with the @QualifiedDo@ language extension to write plans using do
syntax like so:

@
{-# LANGUAGE QualifiedDo #-}
module MyModule where

import qualified Orville.PostgreSQL.Plan.Syntax as PlanSyntax

data FooFamily =
  FooFamily
    { foo :: Foo
    , children :: [FooChildren]
    , pets :: [FooPets]
    }

findFooFamily = PlanSyntax.do $
  fooHeader <- Plan.findOne fooTable fooIdField
  fooChildren <- Plan.findAll fooChildTable fooIdField
  fooPets <- Plan.findAll fooPetTable fooIdField

  FooFamily
    \<$\> Plan.use fooHeader
    \<*\> Plan.use fooChildren
    \<*\> Plan.use fooPets
@

@since 1.0.0.0
-}
{- ORMOLU_ENABLE -}
{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Plan.Syntax
  ( (>>=)
  )
where

import Prelude ()

import qualified Orville.PostgreSQL.Plan as Plan

{- | An operator alias of 'Plan.bind' so that it can be used with @QualifiedDo@.

@since 1.0.0.0
-}
(>>=) ::
  Plan.Plan scope param a ->
  (Plan.Planned scope param a -> Plan.Plan scope param result) ->
  Plan.Plan scope param result
(>>=) = Plan.bind
