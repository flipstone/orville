{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Plan
  ( Plan
  , Planned
  , Execute
  , Explain
  , askParam

    -- * Using a Plan after it is constructed
  , execute
  , explain

    -- * Making a Plan to find rows in the database
  , findMaybeOne
  , findMaybeOneByMarshaller
  , findMaybeOneWhere
  , findMaybeOneWhereByMarshaller
  , findOne
  , findOneByMarshaller
  , findOneShowVia
  , findOneShowViaByMarshaller
  , findOneWhere
  , findOneWhereByMarshaller
  , findOneWhereShowVia
  , findOneWhereShowViaByMarshaller
  , findAll
  , findAllByMarshaller
  , findAllWhere
  , findAllWhereByMarshaller

    -- * Creating a multi-step Plan from other Plan values
  , bind
  , use
  , using
  , chain
  , chainMaybe
  , apply
  , planMany
  , planList
  , planTraversable
  , focusParam
  , planEither
  , planMaybe

    -- * Bridges from other types into Plan
  , Op.AssertionFailed
  , assert
  , planSelect
  , planOperation
  )
where

import Control.Exception (throwIO)
import Control.Monad (join)
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Bifunctor as Bifunctor
import Data.Either (partitionEithers)
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NEL

import Orville.PostgreSQL.Execution (Select)
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Monad as Monad
import qualified Orville.PostgreSQL.Plan.Explanation as Exp
import Orville.PostgreSQL.Plan.Many (Many)
import qualified Orville.PostgreSQL.Plan.Many as Many
import qualified Orville.PostgreSQL.Plan.Operation as Op
import qualified Orville.PostgreSQL.Schema as Schema

{- | A 'Plan' is an executable set of queries that can be executed to load data from the database,
  using the results of prior queries as input parameters to following queries in controlled ways. In
  particular, the "controlled" aspect of this allows plans that take a single input to be adapted to
  take multiple input parameters in a list without the resulting plan executing N+1 queries.  This
  restriction means that while query results can be used as input parameters to later queries, they
  cannot be used to decide to run completely different queries based on other query
  results. Allowing this would prevent the 'Plan' structure from eliminating N+1 query loops.

  Note that during execution, queries are never combined across tables to form joins or
  subqueries. Queries are still executed in the same sequence as specified in the plan, just on all
  the inputs at once rather than in a loop.  If you need to do a join with a plan, you can always
  construct your own custom 'Op.Operation' and use 'planOperation' to incorporate it into a plan.

  The @param@ type variable indicates what type of value is expected as input when the plan is
  executed.

  The @result@ type for a plan indicates what Haskell type is produced when the plan is executed.

  The @scope@ type is used internally by Orville to track how the plan is currently executed against
  a single input or multiple inputs. This type parameter should never be specified as a concrete
  type in user code, but must be exposed as a variable to ensure that execute scope is tracked
  correctly through usages of 'bind'.

@since 1.0.0.0
-}
data Plan scope param result where
  PlanOp :: Op.Operation param result -> Plan scope param result
  PlanMany ::
    (forall manyScope. Plan manyScope param result) ->
    Plan scope [param] (Many param result)
  PlanEither ::
    Plan scope leftParam leftResult ->
    Plan scope rightParam rightResult ->
    Plan scope (Either leftParam rightParam) (Either leftResult rightResult)
  Bind ::
    Plan scope param a ->
    (Planned scope param a -> Plan scope param result) ->
    Plan scope param result
  Use :: Planned scope param a -> Plan scope param a
  Pure :: a -> Plan scope param a
  Apply ::
    Plan scope param (a -> b) ->
    Plan scope param a ->
    Plan scope param b
  Chain ::
    Plan scope a b ->
    Plan scope b c ->
    Plan scope a c

-- | @since 1.0.0.0
instance Functor (Plan scope param) where
  fmap f = Apply (Pure f)

-- | @since 1.0.0.0
instance Applicative (Plan scope param) where
  pure = Pure
  (<*>) = Apply

{- | 'Execute' is a tag type used as the @scope@ variable for 'Plan' values when executing them via
  the 'execute' function.

@since 1.0.0.0
-}
data Execute

{- | 'ExecuteMany' is an internal tag type used by as the @scope@ variable for 'Plan' values when
  executing them against multiple inputs via the 'executeMany' internal function.

@since 1.0.0.0
-}
data ExecuteMany

{- | A 'Planned' value is a wrapper around the results of previously-run queries when using the 'bind'
  function. At the time that you are writing a plan, you do not know whether the 'Plan' will be run
  with a single input or multiple inputs. A 'Planned' value may end up being either an individual
  item or a list of items. Due to this, your ability to interact with the value is limited to the
  use of 'fmap' to extract (or build) other values from the results. 'Planned' values can be used
  together with the 'use' function to make a 'Plan' that produces the extracted value.

  Note that while 'Planned' could provide an 'Applicative' instance as well, it does not to avoid
  confusion with the 'Applicative' instance for 'Plan' itself. If you need to build a value from
  several 'Planned' values using 'Applicative', you should call 'use' on each of the values and use
  the 'Applicative' instance for 'Plan'.

@since 1.0.0.0
-}
data Planned scope param a where
  PlannedOne :: a -> Planned Execute param a
  PlannedMany :: Many k a -> Planned ExecuteMany k a
  PlannedExplain :: Planned Explain param a

instance Functor (Planned scope param) where
  fmap = mapPlanned

{- | 'mapPlanned' applies a function to what value or values have been produced by the plan. This
  function can also be called as 'fmap' or '<$>' thorugh the 'Functor' instance for 'Planned'.

@since 1.0.0.0
-}
mapPlanned :: (a -> b) -> Planned scope param a -> Planned scope param b
mapPlanned f planned =
  case planned of
    PlannedOne a ->
      PlannedOne (f a)
    PlannedMany manyAs ->
      PlannedMany (fmap f manyAs)
    PlannedExplain ->
      PlannedExplain

{- | 'resolveOne' resolves a 'Planned' value that is known to be in the 'Execute' scope to its single
  wrapped value.

@since 1.0.0.0
-}
resolveOne :: Planned Execute param a -> a
resolveOne (PlannedOne a) = a

{- | 'resolveMany resolves a 'Planned' value that is known to be in the 'ExecuteMany' scope to the
  'Many' value wrapped inside it.

@since 1.0.0.0
-}
resolveMany :: Planned ExecuteMany k a -> Many k a
resolveMany (PlannedMany as) = as

{- | 'planOperation' allows any primitive 'Op.Operation' to be used as an atomic step in a plan. When
  the plan is executed, the appropriate 'Op.Operation' functions will be used depending on the
  execution context.

@since 1.0.0.0
-}
planOperation ::
  Op.Operation param result ->
  Plan scope param result
planOperation =
  PlanOp

{- | 'planSelect' allows any Orville 'Select' query to be incorporated into a plan. Note that the
  'Select' cannot depend on the plan's input parameters in this case. If the plan is executed with
  multiple inputs, the same set of all the results will be used as the results for each of the input
  parameters.

@since 1.0.0.0
-}
planSelect :: Select row -> Plan scope () [row]
planSelect select =
  planOperation (Op.findSelect select)

{- | 'askParam' allows the input parameter for the plan to be retrieved as the result of the
  plan. Together with 'bind' you can use this to get access to the input parameter as a 'Planned'
  value.

@since 1.0.0.0
-}
askParam :: Plan scope param param
askParam =
  planOperation Op.askParam

{- | 'findMaybeOne' constructs a 'Plan' that will find at most one row from the given table where the
  plan's input value matches the given database field.

@since 1.0.0.0
-}
findMaybeOne ::
  Ord fieldValue =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Plan scope fieldValue (Maybe readEntity)
findMaybeOne tableDef fieldDef =
  planOperation (Op.findOne tableDef (Op.byField fieldDef))

{- | Construct a plan that will find at most one row from the given table where the plan's input value
  matches the fields in the provided 'Marshall.SqlMarshaller'.

@since 1.1.0.0
-}
findMaybeOneByMarshaller ::
  Ord param =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Plan scope param (Maybe readEntity)
findMaybeOneByMarshaller tableDef paramMarshaller =
  planOperation (Op.findOne tableDef (Op.byMarshaller paramMarshaller))

{- | 'findMaybeOneWhere' is similar to 'findMaybeOne', but allows a 'Expr.BooleanExpr' to be specified
  to restrict which rows are matched by the database query.

@since 1.0.0.0
-}
findMaybeOneWhere ::
  Ord fieldValue =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Expr.BooleanExpr ->
  Plan scope fieldValue (Maybe readEntity)
findMaybeOneWhere tableDef fieldDef cond =
  planOperation (Op.findOneWhere tableDef (Op.byField fieldDef) cond)

{- | Similar to 'findMaybeOneByMarshaller', but allows a 'Expr.BooleanExpr' to be specified to
  restrict which rows are matched by the database query.

@since 1.1.0.0
-}
findMaybeOneWhereByMarshaller ::
  Ord param =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Expr.BooleanExpr ->
  Plan scope param (Maybe readEntity)
findMaybeOneWhereByMarshaller tableDef paramMarshaller cond =
  planOperation (Op.findOneWhere tableDef (Op.byMarshaller paramMarshaller) cond)

{- | 'findOneShowVia' is similar to 'findMaybeOne', but it expects that there will always be a row
  found matching the plan's input value. If no row is found, an 'Op.AssertionFailed' exception will
  be thrown. This is a useful convenience when looking up foreign-key associations that are expected
  to be enforced by the database itself.

@since 1.0.0.0
-}
findOneShowVia ::
  Ord fieldValue =>
  (fieldValue -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Plan scope fieldValue readEntity
findOneShowVia showParam tableDef fieldDef =
  assert
    (assertFound showParam tableDef fieldDef)
    (findMaybeOne tableDef fieldDef)

{- | Similar to 'findMaybeOneByMarshaller', but expects that there will always be a row found matching
  the plan's input value. If no row is found, an 'Op.AssertionFailed' exception will be thrown. This
  is a useful convenience when looking up foreign-key associations that are expected to be enforced
  by the database itself.

@since 1.1.0.0
-}
findOneShowViaByMarshaller ::
  Ord param =>
  (param -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Plan scope param readEntity
findOneShowViaByMarshaller showParam tableDef paramMarshaller =
  assert
    (assertFoundByMarshaller showParam tableDef)
    (findMaybeOneByMarshaller tableDef paramMarshaller)

{- | 'findOne' is an alias to 'findOneShowVia' that uses the 'Show' instance of @fieldValue@ when
  producing a failure message in the event that the entity cannot be found.

@since 1.0.0.0
-}
findOne ::
  (Show fieldValue, Ord fieldValue) =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Plan scope fieldValue readEntity
findOne = findOneShowVia show

{- | An alias for 'findOneShowViaByMarshaller' that uses the 'Show' instance of @param@ when producing
  a failure message in the event that the entity cannot be found.

@since 1.1.0.0
-}
findOneByMarshaller ::
  (Show param, Ord param) =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Plan scope param readEntity
findOneByMarshaller = findOneShowViaByMarshaller show

{- | 'findOneWhereShowVia' is similar to 'findOneShowVia', but allows a 'Expr.BooleanExpr' to be
  specified to restrict which rows are matched by the database query.

@since 1.0.0.0
-}
findOneWhereShowVia ::
  Ord fieldValue =>
  (fieldValue -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Expr.BooleanExpr ->
  Plan scope fieldValue readEntity
findOneWhereShowVia showParam tableDef fieldDef cond =
  assert
    (assertFound showParam tableDef fieldDef)
    (findMaybeOneWhere tableDef fieldDef cond)

{- | Similar to 'findOneShowViaByMarshaller', but allows a 'Expr.BooleanExpr' to be specified to
  restrict which rows are matched by the database query.

@since 1.1.0.0
-}
findOneWhereShowViaByMarshaller ::
  Ord param =>
  (param -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Expr.BooleanExpr ->
  Plan scope param readEntity
findOneWhereShowViaByMarshaller showParam tableDef paramMarshaller cond =
  assert
    (assertFoundByMarshaller showParam tableDef)
    (findMaybeOneWhereByMarshaller tableDef paramMarshaller cond)

{- | 'findOneWhere' is an alias to 'findOneWhereShowVia' that uses the 'Show' instance of @fieldValue@
  when producing a failure message in the event that the entity cannot be found.

@since 1.0.0.0
-}
findOneWhere ::
  (Show fieldValue, Ord fieldValue) =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Expr.BooleanExpr ->
  Plan scope fieldValue readEntity
findOneWhere = findOneWhereShowVia show

{- | An alias for 'findOneWhereShowViaByMarshaller' that uses the 'Show' instance of @param@ when
  producing a failure message in the event that the entity cannot be found.

@since 1.1.0.0
-}
findOneWhereByMarshaller ::
  (Show param, Ord param) =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Expr.BooleanExpr ->
  Plan scope param readEntity
findOneWhereByMarshaller = findOneWhereShowViaByMarshaller show

{- | 'assertFound' is an internal helper that checks that row was found where one was expected.

@since 1.0.0.0
-}
assertFound ::
  (fieldValue -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  fieldValue ->
  Maybe result ->
  Either String result
assertFound showParam tableDef fieldDef param maybeRecord =
  case maybeRecord of
    Just a ->
      Right a
    Nothing ->
      Left $
        unwords
          [ "Failed to find record in table "
          , Schema.tableIdToString $ Schema.tableIdentifier tableDef
          , " where "
          , Marshall.fieldNameToString $ Marshall.fieldName fieldDef
          , " = "
          , showParam param
          ]

{- | An internal helper that checks that a row was found and produces an error message otherwise.

@since 1.1.0.0
-}
assertFoundByMarshaller ::
  (param -> String) ->
  Schema.TableDefinition key writeEntity readEntity ->
  param ->
  Maybe result ->
  Either String result
assertFoundByMarshaller showParam tableDef param maybeRecord =
  case maybeRecord of
    Just a ->
      Right a
    Nothing ->
      Left $
        unwords
          [ "Failed to find record in table "
          , Schema.tableIdToString $ Schema.tableIdentifier tableDef
          , " where param marshaller fields = "
          , showParam param
          ]

{- | 'findAll' constructs a 'Plan' that will find all the rows from the given table where the plan's
  input value matches the given database field.

@since 1.0.0.0
-}
findAll ::
  Ord fieldValue =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Plan scope fieldValue [readEntity]
findAll tableDef fieldDef =
  planOperation (Op.findAll tableDef (Op.byField fieldDef))

{- | Construct a 'Plan' that will find all the rows from the given table where the plan's input value
  matches the fields in the provided 'Marshall.SqlMarshaller'.

@since 1.1.0.0
-}
findAllByMarshaller ::
  Ord param =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Plan scope param [readEntity]
findAllByMarshaller tableDef paramMarshaller =
  planOperation (Op.findAll tableDef (Op.byMarshaller paramMarshaller))

{- | 'findAllWhere' is similar to 'findAll', but allows a 'Expr.BooleanExpr' to be specified to
  restrict which rows are matched by the database query.

@since 1.0.0.0
-}
findAllWhere ::
  Ord fieldValue =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability fieldValue ->
  Expr.BooleanExpr ->
  Plan scope fieldValue [readEntity]
findAllWhere tableDef fieldDef cond =
  planOperation (Op.findAllWhere tableDef (Op.byField fieldDef) cond)

{- | Similar to 'findAllByMarshaller', but allows a 'Expr.BooleanExpr' to be specified to restrict
  which rows are matched by the database query.

@since 1.1.0.0
-}
findAllWhereByMarshaller ::
  Ord param =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.SqlMarshaller param param ->
  Expr.BooleanExpr ->
  Plan scope param [readEntity]
findAllWhereByMarshaller tableDef paramMarshaller cond =
  planOperation (Op.findAllWhere tableDef (Op.byMarshaller paramMarshaller) cond)

{- | 'planMany' adapts a plan that takes a single input parameter to work on multiple input
  parameters. When the new plan is executed, each query will execute in the same basic order, but
  with adjusted conditions to find all the rows for all inputs at once rather than running the
  planned queries once for each input.

@since 1.0.0.0
-}
planMany ::
  (forall manyScope. Plan manyScope param result) ->
  Plan scope [param] (Many param result)
planMany =
  PlanMany

{- | 'planList' lifts a plan so both its param and result become lists. This saves you from having to
  fmap in 'Many.elems' when all you want back from a 'Many' is the list of results inside it.

  There will always be the same number of elements in the @[result]@ list as there are in the
  @[param]@ list, even if there are duplicate values in the input parameters. This may be
  counter-intuitive in the trivial case where a plan that queries a single table is passed to
  'planList' but cannot be avoided due to more complicated situations where the original plan
  executes queries against multiple tables. When a plan that queries multiple tables is passed, the
  query results must be correlated based on the input parameters to build each @result@ value.

@since 1.0.0.0
-}
planList ::
  (forall scope. Plan scope param result) ->
  Plan listScope [param] [result]
planList plan =
  Many.elems <$> planMany plan

{- | Similar to 'planList', but generalized to work with any 'Traversable'.

@since 1.1.0.0
-}
planTraversable ::
  forall t tScope param result.
  Traversable t =>
  (forall scope. Plan scope param result) ->
  Plan tScope (t param) (t result)
planTraversable plan =
  let
    lookupAll :: t param -> Many param result -> Either String (t result)
    lookupAll t m =
      traverse (Bifunctor.first (const "planTraversable invariant violated: Missing Key") . flip Many.lookup m) t
  in
    assert lookupAll $ chain (fmap Foldable.toList askParam) (planMany plan)

{- | 'focusParam' builds a plan from a function and an existing plan, taking the result of that
  function as input. This is especially useful when there is some structure, and a plan that only
  needs a part of that structure as input.  The function argument can access part of the structure
  for the plan argument to use, so the final returned plan can take the entire structure as input.

@since 1.0.0.0
-}
focusParam ::
  (a -> b) ->
  Plan scope b result ->
  Plan scope a result
focusParam focuser =
  chain (fmap focuser askParam)

{- | 'planEither' lets you construct a plan that branches by executing a different plan for the 'Left'
  and 'Right' sides of an 'Either' value. When used with a single input parameter, only one of the
  two plans will be used, based on the input parameter. When used on multiple input parameters, each
  of the two plans will be executed only once with all the 'Left' and 'Right' values provided as
  input parameters respectively.

@since 1.0.0.0
-}
planEither ::
  Plan scope leftParam leftResult ->
  Plan scope rightParam rightResult ->
  Plan scope (Either leftParam rightParam) (Either leftResult rightResult)
planEither =
  PlanEither

{- | 'planMaybe' lifts a plan so both its param and result become 'Maybe's. This is useful when
  modifying an existing plan to deal with optionality. Writing just one plan can then easily produce
  both the required and optional versions.

@since 1.0.0.0
-}
planMaybe :: Plan scope a b -> Plan scope (Maybe a) (Maybe b)
planMaybe plan =
  focusParam (maybe (Left ()) Right) $
    either id id <$> planEither (pure Nothing) (Just <$> plan)

{- | 'bind' gives access to the results of a plan to use as input values to future plans. The plan
  result is given the input parameter to the provided function, which must produce the remaining
  'Plan' to be executed. The value will be wrapped in the 'Planned' type, which may represent either
  a result or multiple results, depending on whether one plan is currently being executed with one
  and multiple input parameters. This ensures that the caller produces only a single remaining
  'Plan' to be used for all inputs when there are multiple to eliminate the need to possibly run
  different queries for different inputs (which would an introduce N+1 query execution).

  The 'Planned' value (or values) provided by 'bind' have actually been retrieved from the database,
  so the value can be used multiple times when constructing the remaining 'Plan' without fear of
  causing the query to run multiple times.

  Also see 'use' for how to lift a 'Planned' value back into a 'Plan'.

@since 1.0.0.0
-}
bind ::
  Plan scope param a ->
  (Planned scope param a -> Plan scope param result) ->
  Plan scope param result
bind =
  Bind

{- | 'use' constructs a 'Plan' that always produces the 'Planned' value as its result, regardless of
  the parameter given as input to the plan.

@since 1.0.0.0
-}
use :: Planned scope param a -> Plan scope param a
use =
  Use

{- | 'using' uses a 'Planned' value in the input to another 'Plan'. The resulting plan will ignore its
  input and use the 'Planned' value as the input to produce its result instead.

@since 1.0.0.0
-}
using ::
  Planned scope param a ->
  Plan scope a b ->
  Plan scope param b
using planned =
  chain (use planned)

{- | 'apply' applies a function produced by a plan to the value produced by another plan. This is
  usually used via the '<*>' operator through the 'Applicative' instance for 'Plan'.

@since 1.0.0.0
-}
apply ::
  Plan scope param (a -> b) ->
  Plan scope param a ->
  Plan scope param b
apply =
  Apply

{- | 'chain' connects the output of one plan to the input of another to form a larger plan that will
  execute the first followed by the second.

@since 1.0.0.0
-}
chain ::
  Plan scope a b ->
  Plan scope b c ->
  Plan scope a c
chain =
  Chain

{- | 'chainMaybe' connects two plans that both yield Maybes.  If the first plan yields no result, the
  second is skipped.  See also 'chain'.

@since 1.0.0.0
-}
chainMaybe ::
  Plan scope a (Maybe b) ->
  Plan scope b (Maybe c) ->
  Plan scope a (Maybe c)
chainMaybe aPlan bPlan =
  let
    optionalInput ::
      Plan scope a (Maybe b) ->
      Plan scope (Maybe a) (Maybe b)
    optionalInput =
      fmap join . planMaybe
  in
    Chain aPlan (optionalInput bPlan)

{- | 'assert' allows you to make an assertion about a plan's result that will throw an
  'Op.AssertionFailed' exception during execution if it proves to be false. The first parameter is
  the assertion function, which should return either an error message to be given in the exception
  or the value to be used as the plan's result.

@since 1.0.0.0
-}
assert ::
  (param -> a -> Either String b) ->
  Plan scope param a ->
  Plan scope param b
assert assertion aPlan =
  let
    eitherPlan =
      assertion
        <$> askParam
        <*> aPlan
  in
    chain eitherPlan (PlanOp Op.assertRight)

{- | 'execute' accepts the input parameter (or parameters) expected by a 'Plan' and runs the plan to
  completion, either throwing an 'Op.AssertionFailed' exception in the monad @m@ or producing the
  expected result.

  If you have a plan that takes one input and want to provide a list of input, use 'planMany' to
  adapt it to a multple-input plan before calling 'execute'.

@since 1.0.0.0
-}
execute ::
  Monad.MonadOrville m =>
  Plan Execute param result ->
  param ->
  m result
execute =
  executeOne

{- | 'executeOne' is an internal helper that executes a 'Plan' with a concrete @scope@ type to ensure
  all 'Planned' values are built with 'PlannedOne'.

@since 1.0.0.0
-}
executeOne ::
  Monad.MonadOrville m =>
  Plan Execute param result ->
  param ->
  m result
executeOne plan param =
  case plan of
    PlanOp operation -> do
      opResult <- Op.executeOperationOne operation param

      case opResult of
        Left err ->
          MIO.liftIO (throwIO err)
        Right result ->
          pure result
    PlanMany manyPlan ->
      executeMany manyPlan param
    PlanEither leftPlan rightPlan ->
      case param of
        Left leftParam ->
          Left <$> executeOne leftPlan leftParam
        Right rightParam ->
          Right <$> executeOne rightPlan rightParam
    Bind intermPlan continue -> do
      interm <- executeOne intermPlan param
      executeOne
        (continue (PlannedOne interm))
        param
    Use planned ->
      pure . resolveOne $ planned
    Pure a ->
      pure a
    Apply planF planA ->
      executeOne planF param <*> executeOne planA param
    Chain planAB planBC -> do
      b <- executeOne planAB param
      executeOne planBC b

{- | 'executeMany' is an internal helper that executes a 'Plan' with a concrete @scope@ type to ensure
  all 'Planned' values are built with 'PlannedMany'.

@since 1.0.0.0
-}
executeMany ::
  Monad.MonadOrville m =>
  Plan ExecuteMany param result ->
  [param] ->
  m (Many.Many param result)
executeMany plan params =
  case plan of
    PlanOp operation -> do
      case NEL.nonEmpty params of
        Nothing ->
          pure $ Many.fromKeys params (const $ Left Many.NotAKey)
        Just nonEmptyParams -> do
          opResult <- Op.executeOperationMany operation nonEmptyParams

          case opResult of
            Left err ->
              MIO.liftIO (throwIO err)
            Right results ->
              pure results
    PlanMany manyPlan -> do
      let
        flatParams = concat params

      allResults <- executeMany manyPlan flatParams

      let
        restrictResults subParams =
          Many.fromKeys subParams (\k -> Many.lookup k allResults)

      pure $ Many.fromKeys params (Right . restrictResults)
    PlanEither leftPlan rightPlan -> do
      let
        (leftParams, rightParams) = partitionEithers params

      leftResults <- executeMany leftPlan leftParams
      rightResults <- executeMany rightPlan rightParams

      let
        eitherResult eitherK =
          case eitherK of
            Left k ->
              Left <$> Many.lookup k leftResults
            Right k ->
              Right <$> Many.lookup k rightResults

      pure $ Many.fromKeys params eitherResult
    Bind intermPlan continue -> do
      interms <- executeMany intermPlan params
      executeMany
        (continue (PlannedMany interms))
        params
    Use planned ->
      pure . resolveMany $ planned
    Pure a ->
      pure $ Many.fromKeys params (const (Right a))
    Apply planF planA -> do
      manyFs <- executeMany planF params
      manyAs <- executeMany planA params

      pure (Many.apply manyFs manyAs)
    Chain planAB planBC -> do
      bs <- executeMany planAB params
      cs <- executeMany planBC (Many.elems bs)
      pure $ Many.compose cs bs

{- | 'Explain' is a tag type used as the @scope@ variable when explaining a 'Plan' via the 'explain'
  function.

@since 1.0.0.0
-}
data Explain
  = ExplainOne
  | ExplainMany

{- | 'explain' produces a textual description of the steps outlined by a 'Plan' -- in most cases
  example SQL queries. If you want to see the explanation of how the plan will run with multiple
  input parameters, you can use 'planMany' to adapt it before calling 'explain'.

@since 1.0.0.0
-}
explain :: Plan Explain param result -> [String]
explain plan =
  Exp.explanationSteps $
    explainPlan ExplainOne plan

{- | 'explainPlan' is an internal helper to executes a plan with the @scope@ type fixed to 'Explain'
  to ensure that all 'Planned' values are constructed with the 'PlannedExplain' constructor.

@since 1.0.0.0
-}
explainPlan ::
  Explain ->
  Plan Explain param result ->
  Exp.Explanation
explainPlan mult plan =
  case plan of
    PlanOp operation -> do
      case mult of
        ExplainOne ->
          Op.explainOperationOne operation
        ExplainMany ->
          Op.explainOperationMany operation
    PlanMany manyPlan -> do
      explainPlan ExplainMany manyPlan
    PlanEither leftPlan rightPlan ->
      explainPlan mult leftPlan <> explainPlan mult rightPlan
    Bind intermPlan continue ->
      let
        nextPlan = continue PlannedExplain
      in
        explainPlan mult intermPlan <> explainPlan mult nextPlan
    Use _ ->
      Exp.noExplanation
    Pure _ ->
      Exp.noExplanation
    Apply planF planA -> do
      explainPlan mult planF <> explainPlan mult planA
    Chain planAB planBC -> do
      explainPlan mult planAB <> explainPlan mult planBC
