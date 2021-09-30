{- |
Module    : Orville.PostgreSQL.Connection
Copyright : Flipstone Technology Partners 2021
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
    <$> Plan.use fooHeader
    <*> Plan.use fooChildren
    <*> Plan.use fooPets
@

-}
module Database.Orville.PostgreSQL.Plan.Syntax
  ( (>>=),
  )
where

import Prelude ()

import qualified Database.Orville.PostgreSQL.Plan as Plan

{- |
  An operator alias of 'Plan.bind' so that it can be used with @QualifiedDo@.
-}
(>>=) ::
  Plan.Plan scope param a ->
  (Plan.Planned scope param a -> Plan.Plan scope param result) ->
  Plan.Plan scope param result
(>>=) = Plan.bind
