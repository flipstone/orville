{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Database.Orville.PostgreSQL.Plan
  ( Plan
  , askParam

  , findMaybeOne
  , findMaybeOneWhere
  , findOne
  , findOneWhere
  , findAll
  , findAllWhere

  , apply
  , bind
  , chain
  , use
  , using
  , planMany

  , execute
  , explain

  , assert
  , planSelect
  , planOperation
  ) where

import qualified Control.Monad.Catch as Catch

import qualified Database.Orville.PostgreSQL.Core as Core
import           Database.Orville.PostgreSQL.Internal.MappendCompat ((<>))
import           Database.Orville.PostgreSQL.Plan.Many (Many)
import qualified Database.Orville.PostgreSQL.Plan.Many as Many
import qualified Database.Orville.PostgreSQL.Plan.Operation as Op
import qualified Database.Orville.PostgreSQL.Plan.Explanation as Exp
import           Database.Orville.PostgreSQL.Select (Select)

{-|
  A 'Plan' is an executable set of queries that can be executed to load data
  from the database, using the results of prior queries as input parameters to
  following queries in controlled ways. In particular, the "controlled" aspect
  of this allows plans that take a single input to be adapted to take multiple
  input parameters in a list without the resulting plan executing N+1 queries.
  This restriction means that while query results can be used as input
  parameters to later queries, they cannot be used to decide to run completely
  different queries based on other query results. Allowing this would prevent
  the 'Plan' structure from eliminating N+1 query loops.

  Note that during execution queries are never combined across tables to form
  joins or subqueries. Queries are still executed in the same sequence as
  specified in the plan, just on all the inputs at once rather than in a loop.
  If you need to do a join with a plan, you can always construction your
  own custom 'Operation' and use 'planOperation' to incorporate into a plan.

  The @param@ type variable indicates what type of value is expected as input
  when the plan is executed.

  The @result@ type for a plan indicates what Haskell type is produced
  when the plan is executed.

  The @scope@ type is used internally by Orville to track the plan is currently
  executed against a single input or multiple inputs. This type parameter
  should never specified as a concrete type in user code, but must be exposed
  as a variable to ensure that execute scope is tracked correctly through
  usages of 'bind'.

-}
data Plan scope param result where
  PlanOp :: Op.Operation param result -> Plan scope param result

  PlanMany :: (forall manyScope. Plan manyScope param result)
           -> Plan scope [param] (Many param result)

  Bind :: Plan scope param a
       -> (Planned scope a -> Plan scope param result)
       -> Plan scope param result

  Use :: Planned scope a -> Plan scope param a

  Pure :: a -> Plan scope param a

  Apply :: Plan scope param (a -> b)
        -> Plan scope param a
        -> Plan scope param b

  Chain :: Plan scope a b
        -> (forall chainedScope. Plan chainedScope b c)
        -> Plan scope a c

instance Functor (Plan scope param) where
  fmap f = Apply (Pure f)

instance Applicative (Plan scope param) where
  pure = Pure
  (<*>) = Apply

{-|
  'OneScope' is an internal tag type used by as the 'scope' variable for
  'Plan' values when executing them against a single input.
-}
data OneScope

{-|
  'ManyScope is an internal tag type used by as the 'scope' variable for
  'Plan' values when executing them against multiple inputs.
-}
data ManyScope k

{-|
  A 'Planned' value is a wrapper around the results of previous run queries
  when using the 'bind' function. At the time that you are writing a plan you
  do not know whether the 'Plan' will be run with a single input or multiple
  inputs. A 'Planned' value may end up being either an individual item or a
  list of items. Due to this, your ability to interact with the value is
  limited to the use of 'fmap' to extract (or build) other values from the
  results. 'Planned' values can be used together with the 'use' function to
  make a 'Plan' that produces the extracted value.

  Note that while 'Planned' could provide an 'Applicative' instance as well, it
  does not to avoid confusion with 'Applicative' instance for 'Plan' itself.
  If you need to build a value from several 'Planned' values using
  'Applicative', you should call 'use' on each of the values and use the
  'Applicative' instance for 'Plan'.
-}
data Planned scope a where
  PlannedOne      :: a -> Planned OneScope a
  PlannedMany     :: Many k a -> Planned (ManyScope k) a
  PlannedExplain  :: Planned Explain a

instance Functor (Planned scope) where
  fmap = mapPlanned

{-|
  'mapPlanned' applies a function to what value or values have been produced by
  the plan. This function can also be called as 'fmap' or '<$>' thorugh the
  'Functor' instance for 'Planned'.
-}
mapPlanned :: (a -> b) -> Planned scope a -> Planned scope b
mapPlanned f planned =
  case planned of
    PlannedOne a ->
      PlannedOne (f a)

    PlannedMany manyAs ->
      PlannedMany (fmap f manyAs)

    PlannedExplain ->
      PlannedExplain

{-|
  'resolveOne' resolves a 'Planned' value that is known to be in the 'One'
  scope to its single wrapped value.
-}
resolveOne :: Planned OneScope a -> a
resolveOne (PlannedOne a) = a

{-|
  'resolveMany resolves a 'Planned' value that is known to be in the 'Many'
  scope to the 'Many' value wrapped inside it.
-}
resolveMany :: Planned (ManyScope k) a -> Many k a
resolveMany (PlannedMany as) = as

{-|
  'planOperation' allows any primitive 'Operation' to be used as an atomic step
  in a plan. When the plan is executed, the appropriate 'Operation' functions
  will be used depending on the execution context.
-}
planOperation :: Op.Operation param result
              -> Plan scope param result
planOperation =
  PlanOp

{-|
  'planSelect' allows any Orville 'Select' query to be incorporated into a
  plan. Note that the 'Select' cannot depend on the plan's input parameters in
  this case. If the plan is executed with multiple inputs the same set of all
  the results will be used as the results for each of the input parameters.
-}
planSelect :: Select row -> Plan scope param [row]
planSelect select =
  planOperation (Op.findSelect select)

{-|
  'askParam' allows the input parameter for the plan to be retrieved as the
  result of the plan. Together with 'bind' you can use this to get access to
  the input parameter as a 'Planned' value.
-}
askParam :: Plan scope param param
askParam =
  planOperation Op.askParam

{-|
  'findMaybeOne' constructs a 'Plan' that will find at most one row from
  the given table where the plan's input value matches the given database
  field.
-}
findMaybeOne :: Ord fieldValue
             => Core.TableDefinition readEntity writeEntity key
             -> Core.FieldDefinition nullability fieldValue
             -> Plan scope fieldValue (Maybe readEntity)
findMaybeOne tableDef fieldDef =
  planOperation (Op.findOne tableDef fieldDef)

{-|
  'findMaybeOneWhere' is similar to 'findMaybeOne', but allows a
  'WhereCondition' to be specified to restrict which rows are matched by the
  database query.
-}
findMaybeOneWhere :: Ord fieldValue
                  => Core.TableDefinition readEntity writeEntity key
                  -> Core.FieldDefinition nullability fieldValue
                  -> Core.WhereCondition
                  -> Plan scope fieldValue (Maybe readEntity)
findMaybeOneWhere tableDef fieldDef cond =
  planOperation (Op.findOneWhere tableDef fieldDef cond)

{-|
  'findOne' is similar to 'findOneMaybe', but it expects that there will always
  be a row found matching the plan's input value. If no row is found an
  'AssertionFailed' exception will be thrown. This is a useful convenience
  when looking up foreign-key associations that are expected to be enforced
  by the database itself.
-}
findOne :: (Show fieldValue, Ord fieldValue)
        => Core.TableDefinition readEntity writeEntity key
        -> Core.FieldDefinition nullability fieldValue
        -> Plan scope fieldValue readEntity
findOne tableDef fieldDef =
  assert
    (assertFound tableDef fieldDef)
    (findMaybeOne tableDef fieldDef)

{-|
  'findOneWhere' is similar to 'findOne', but allows a 'WhereCondition' to be
  specified to restrict which rows are matched by the database query.
-}
findOneWhere :: (Show fieldValue, Ord fieldValue)
             => Core.TableDefinition readEntity writeEntity key
             -> Core.FieldDefinition nullability fieldValue
             -> Core.WhereCondition
             -> Plan scope fieldValue readEntity
findOneWhere tableDef fieldDef cond =
  assert
    (assertFound tableDef fieldDef)
    (findMaybeOneWhere tableDef fieldDef cond)

{-|
  'assertFound' is an internal helper that checks that row was found where
  one was expected.
-}
assertFound :: Show fieldValue
            => Core.TableDefinition readEntity writeEntity key
            -> Core.FieldDefinition nullability fieldValue
            -> fieldValue
            -> Maybe result
            -> Either String result
assertFound tableDef fieldDef param maybeRecord =
  case maybeRecord of
    Just a ->
      Right a

    Nothing ->
      Left $
        unwords
          [ "Failed to find record in table"
          , Core.tableName tableDef
          , "where"
          , Core.fieldName fieldDef
          , " = "
          , show param
          ]

{-|
  'findAll' constructs a 'Plan' that will find all the rows from the given
  table there the plan's input value matches the given database field.
-}
findAll :: Ord fieldValue
        => Core.TableDefinition readEntity writeEntity key
        -> Core.FieldDefinition nullability fieldValue
        -> Plan scope fieldValue [readEntity]
findAll tableDef fieldDef =
  planOperation (Op.findAll tableDef fieldDef)

{-|
  'findAllWhere' is similar to 'findAll', but allows a 'WhereCondition' to be
  specified to restrict which rows are matched by the database query.
-}
findAllWhere :: Ord fieldValue
             => Core.TableDefinition readEntity writeEntity key
             -> Core.FieldDefinition nullability fieldValue
             -> Core.WhereCondition
             -> Plan scope fieldValue [readEntity]
findAllWhere tableDef fieldDef cond =
  planOperation (Op.findAllWhere tableDef fieldDef cond)

{-|
  'planMany' adapts a plan that takes a single input parameter to work on
  multiple input parameters. When the new plan is executed each query will
  execute in the same basic order, but with adjusted conditions to find all the
  rows for all inputs at once rather than running the planned queries once for
  each input.
-}
planMany :: (forall manyScope. Plan manyScope param result)
         -> Plan scope [param] (Many param result)
planMany =
  PlanMany

{-|
  'bind' gives access to the results of a plan to use as input values to future
  plans. The plan result is given the input parameter to the provided function,
  which must produce the remaining 'Plan' to be executed. The value will be
  wrapped in the 'Planned' type, which may represent either a result or
  multiple results, depending on whether one plan is currently be executed with
  one and multiple input parameters. This ensures that the caller produces only
  a single remaining 'Plan' to be used for all inputs when there are multiple
  to eliminate the need to possibly run different queries for different inputs
  (which would an introduce N+1 query execution).

  The 'Planned' value (or values) provided by 'bind' have actually been
  retrieved from the database, so the value can be used multiple times when
  constructing the remaining 'Plan' without fear of causing the query to run
  multiple times.

  Also see 'use' for how to lift a 'Planned' value back into a 'Plan'.
-}
bind :: Plan scope param a
     -> (Planned scope a -> Plan scope param result)
     -> Plan scope param result
bind =
  Bind

{-|
  'use' constructs a 'Plan' that always produces the 'Planned' value
  as its result, regardless of the parameter given as input to the plan.
-}
use :: Planned scope a -> Plan scope param a
use =
  Use

{-|
  'using' uses a 'Planned' value in the input to another 'Plan'. The
  resulting plan will ignore its input and use the 'Planned' value as
  the input to produce its result instead.
-}
using :: Planned scope a
      -> (forall usingScope. Plan usingScope a b)
      -> Plan scope param b
using planned plan =
  chain (use planned) plan

{-|
  'apply' applies a function produced by a plan to the value produced
  by another plan. This is usually used via the '<*>' operator through
  the 'Applicative' instance for 'Plan'.
-}
apply :: Plan scope param (a -> b)
      -> Plan scope param a
      -> Plan scope param b
apply =
  Apply

{-|
  'chain' connects the output of one plan to the input of another to form a
  larger plan that will execute the first followed by the second.
-}
chain :: Plan scope a b
      -> (forall chainedScope. Plan chainedScope b c)
      -> Plan scope a c
chain =
  Chain

{-|
  'assert' allows you to make an assertion about a plans result that will throw
  an 'AssertionFailed' failed exception during execution if it proves to be
  false. The first parameter is the assertion function, which should return
  either an error message to be given in the exception or the value to be used
  as the plan's result.
-}
assert :: (param -> a -> Either String b)
       -> Plan scope param a
       -> Plan scope param b
assert assertion aPlan =
  let
    eitherPlan =
      assertion
        <$> askParam
        <*> aPlan
  in
    chain eitherPlan (PlanOp Op.assertRight)

{-|
  'execute' accepts the input parameter (or parameters) expected by a 'Plan'
  and runs the plan to completion, either throwing an 'AssertionFailed'
  exception in the monad 'm' or producing the expected result.

  If you have a plan that takes one input and want to provide a list of
  input, use 'planMany' to adapt it to a multple-input plan before calling
  'execute'.
-}
execute :: Core.MonadOrville conn m
        => (forall scope. Plan scope param result)
        -> param
        -> m result
execute plan param =
  executeOne plan param

{-|
  'executeOne' is an internal helper that executes a 'Plan' with a concrete
  'scope' type to ensure all 'Planned' values are built with 'PlannedOne'.
-}
executeOne :: Core.MonadOrville conn m
           => Plan OneScope param result
           -> param
           -> m result
executeOne plan param =
  case plan of
    PlanOp operation -> do
      opResult <- Op.executeOperationOne operation param

      case opResult of
        Left err ->
          Catch.throwM err

        Right result ->
          pure result

    PlanMany manyPlan ->
      executeMany manyPlan param

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

{-|
  'executeMany' is an internal helper that executes a 'Plan' with a concrete
  @scope@ type to ensure all 'Planned' values are built with 'PlannedMany'.
-}
executeMany :: Core.MonadOrville conn m
            => Plan (ManyScope param) param result
            -> [param]
            -> m (Many.Many param result)
executeMany plan params =
  case plan of
    PlanOp operation -> do
      opResult <- Op.executeOperationMany operation params

      case opResult of
        Left err ->
          Catch.throwM $ err

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

{-|
  'Explain' is an internal type used to track whether an explanation is
  being generated for a single input or multiple inputs.
-}
data Explain
  = ExplainOne
  | ExplainMany

{-|
  'explain' produces a textual description of the steps outlined by
  a 'Plan' -- in most cases example SQL queries. If you want to see
  the explanation of how the plan will run with multiple input parameters,
  you can use 'planMany' to adapt it before calling 'explain'.
-}
explain :: (forall scope. Plan scope param result) -> [String]
explain plan =
  Exp.explanationSteps $
    explainPlan ExplainOne plan

{-|
  'explainPlan' is an internal helper to executes a plan with the
  'scope' type fixed to 'Explain' to ensure that all 'Planned'
  values are constructed with the 'PlannedExplain' constructor.
-}
explainPlan :: Explain
            -> Plan Explain param result
            -> Exp.Explanation
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

