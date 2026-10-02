{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- | A scratch prototype of a restricted plan DSL that can either be embedded
  into Orville's native 'Plan.Plan' (via 'toPlan') or compiled to a single SQL
  query that chains CTEs and returns nested results as jsonb.

  The language is a sequence of let-bindings written with @QualifiedDo@: each
  bound step becomes a named CTE, and later steps refer to earlier results
  through opaque references, mirroring how Orville's native plan @bind@ hands
  out 'Plan.Planned' values. References are parametric (the plan type is
  abstract in the reference type), so a plan can be interpreted with
  references instantiated as CTE names, as result decoders, or as native
  'Plan.Planned' values.

  Every value crossing a step boundary server-side is represented as jsonb.
  Scalar and column values are cast to @text@ before being placed into jsonb,
  so that the strings coming back are byte-identical to what libpq's text mode
  would deliver. Decoding therefore reuses the table's existing
  'Marshall.SqlMarshaller' by replaying the strings through a
  'Exec.mkFakeLibPQResult'.
-}
module Orville.JsonPlan
  ( JsonPlan
  , (>>=)
  , (>>)
  , findOne
  , findMaybeOne
  , findAll
  , findAllWhere
  , findAllEach
  , selectWhere
  , use
  , pair
  , JsonResult
  , refValue
  , result
  , Arg
  , rootParam
  , refField
  , toPlan
  , executeJsonPlan
  , executeJsonPlanList
  , compiledSqlText
  , JsonQuery
  , jsonQuery
  , jsonQueryToPlan
  , executeJsonQuery
  , executeJsonQueryList
  , jsonQuerySqlText
  , JsonPlanError (..)
  , renderJsonPlanError
  , DecodeError (..)
  , renderDecodeError
  ) where

import Prelude hiding ((>>), (>>=))

import Control.Exception (Exception (displayException), throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NEL
import qualified Data.Profunctor as Profunctor
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Vector as Vector

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Schema as Schema

{- | The restricted plan vocabulary. Steps are sequenced with '(>>=)' (via
  @QualifiedDo@), which hands the body an opaque @ref@ naming the step's
  result. A plan must be parametric in @ref@ to be executed or embedded,
  which prevents references from escaping their plan or being inspected.
-}
data JsonPlan ref param result where
  FindOne ::
    (Show a, Ord a) =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability a ->
    Arg ref param a ->
    JsonPlan ref param readEntity
  FindMaybeOne ::
    Ord a =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability a ->
    Arg ref param a ->
    JsonPlan ref param (Maybe readEntity)
  FindAll ::
    Ord a =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability a ->
    Arg ref param a ->
    JsonPlan ref param [readEntity]
  FindAllWhere ::
    Ord a =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability a ->
    Expr.BooleanExpr ->
    Arg ref param a ->
    JsonPlan ref param [readEntity]
  SelectWhere ::
    Schema.TableDefinition key writeEntity readEntity ->
    Expr.BooleanExpr ->
    JsonPlan ref param [readEntity]
  FindAllEach ::
    Ord a =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability a ->
    Arg ref param a ->
    (forall innerRef innerParam. innerRef readEntity -> JsonPlan innerRef innerParam b) ->
    JsonPlan ref param [b]
  Bind ::
    JsonPlan ref param a ->
    (ref a -> JsonPlan ref param b) ->
    JsonPlan ref param b
  Use ::
    ref a ->
    JsonPlan ref param a
  Result ::
    JsonResult ref a ->
    JsonPlan ref param a

{- | Assembles a plan's (or block's) result from previously bound references
  and pure values, via the 'Applicative' instance. The combining functions
  run client-side after decoding, so they are unrestricted; server-side the
  assembly compiles to a single jsonb object holding each referenced value.
-}
data JsonResult ref a where
  UseRef :: ref a -> JsonResult ref a
  PureResult :: a -> JsonResult ref a
  ApplyResult ::
    JsonResult ref (a -> b) ->
    JsonResult ref a ->
    JsonResult ref b

instance Functor (JsonResult ref) where
  fmap f =
    ApplyResult (PureResult f)

instance Applicative (JsonResult ref) where
  pure = PureResult
  (<*>) = ApplyResult

{- | The lookup argument of a step: either the plan's input parameter or a
  field projected out of a previously bound result.
-}
data Arg ref param a where
  RootParam :: Arg ref param param
  RefField ::
    (b -> a) ->
    Marshall.FieldDefinition nullability a ->
    ref b ->
    Arg ref param a

{- | Binds a step's result for use in the rest of the plan. Written as
  @x <- step@ inside a qualified @do@ block.
-}
(>>=) ::
  JsonPlan ref param a ->
  (ref a -> JsonPlan ref param b) ->
  JsonPlan ref param b
(>>=) =
  Bind

-- | Sequences a step whose result is not needed later.
(>>) ::
  JsonPlan ref param a ->
  JsonPlan ref param b ->
  JsonPlan ref param b
(>>) step rest =
  Bind step (\_ -> rest)

-- | Finds the single row whose field matches the argument, failing if none does.
findOne ::
  (Show a, Ord a) =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Arg ref param a ->
  JsonPlan ref param readEntity
findOne =
  FindOne

{- | Finds the single row whose field matches the argument, or 'Nothing' if
  none does. Unlike 'findOne', a missing row is a result rather than an
  error.
-}
findMaybeOne ::
  Ord a =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Arg ref param a ->
  JsonPlan ref param (Maybe readEntity)
findMaybeOne =
  FindMaybeOne

-- | Finds all rows whose field matches the argument.
findAll ::
  Ord a =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Arg ref param a ->
  JsonPlan ref param [readEntity]
findAll =
  FindAll

{- | Finds all rows whose field matches the argument and that also satisfy the
  given condition.
-}
findAllWhere ::
  Ord a =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Expr.BooleanExpr ->
  Arg ref param a ->
  JsonPlan ref param [readEntity]
findAllWhere =
  FindAllWhere

{- | Finds all rows whose field matches the argument and runs a sub-plan for
  each found row, producing the sub-plan results in one list. The sub-plan
  receives the found row as a bound reference and is parametric in its
  reference brand and parameter type, so it can only reach its own bindings:
  outer references and the outer parameter are out of scope by construction.
-}
findAllEach ::
  Ord a =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Arg ref param a ->
  (forall innerRef innerParam. innerRef readEntity -> JsonPlan innerRef innerParam b) ->
  JsonPlan ref param [b]
findAllEach =
  FindAllEach

{- | Finds all rows satisfying the given condition, independent of the plan's
  parameter and of any bound reference.
-}
selectWhere ::
  Schema.TableDefinition key writeEntity readEntity ->
  Expr.BooleanExpr ->
  JsonPlan ref param [readEntity]
selectWhere =
  SelectWhere

-- | Produces a previously bound result as the plan's (or block's) result.
use :: ref a -> JsonPlan ref param a
use =
  Use

-- | A previously bound result, for use in a 'result' assembly.
refValue :: ref a -> JsonResult ref a
refValue =
  UseRef

-- | Produces an assembly of previously bound results as the plan's result.
result :: JsonResult ref a -> JsonPlan ref param a
result =
  Result

-- | Produces two previously bound results as a tuple.
pair :: ref a -> ref b -> JsonPlan ref param (a, b)
pair refA refB =
  Result ((,) <$> UseRef refA <*> UseRef refB)

-- | The plan's input parameter, used as a step's lookup argument.
rootParam :: Arg ref param param
rootParam =
  RootParam

{- | A field of a previously bound result, used as a step's lookup argument.
  Carries both interpretations: a Haskell accessor for native execution, and
  the field whose column name and SQL type give the jsonb path and cast for
  compiled execution. The caller is trusted to keep the two in agreement, the
  same contract 'Marshall.marshallField' already relies on.
-}
refField ::
  (b -> a) ->
  Marshall.FieldDefinition nullability a ->
  ref b ->
  Arg ref param a
refField =
  RefField

--
-- Embedding into the native plan language
--

{- | Embeds a 'JsonPlan' into Orville's native plan language. References are
  interpreted directly as native 'Plan.Planned' values: 'Bind' maps onto
  'Plan.bind' and 'use' onto 'Plan.use'.
-}
toPlan ::
  (forall ref. JsonPlan ref param result) ->
  Plan.Plan scope param result
toPlan plan =
  toPlanNode plan

toPlanNode ::
  JsonPlan (Plan.Planned scope param) param result ->
  Plan.Plan scope param result
toPlanNode plan =
  case plan of
    FindOne tableDef fieldDef arg ->
      Plan.chain (argToPlan arg) (Plan.findOne tableDef fieldDef)
    FindMaybeOne tableDef fieldDef arg ->
      Plan.chain (argToPlan arg) (Plan.findMaybeOne tableDef fieldDef)
    FindAll tableDef fieldDef arg ->
      Plan.chain (argToPlan arg) (Plan.findAll tableDef fieldDef)
    FindAllWhere tableDef fieldDef cond arg ->
      Plan.chain (argToPlan arg) (Plan.findAllWhere tableDef fieldDef cond)
    SelectWhere tableDef cond ->
      Plan.focusParam (const ()) $
        Plan.planSelect (Exec.selectTable tableDef (O.where_ cond))
    FindAllEach tableDef fieldDef arg innerPlan ->
      Plan.chain
        (Plan.chain (argToPlan arg) (Plan.findAll tableDef fieldDef))
        (Plan.planList (Plan.bind Plan.askParam (toPlanNode . innerPlan)))
    Bind step continue ->
      Plan.bind (toPlanNode step) (toPlanNode . continue)
    Use planned ->
      Plan.use planned
    Result jsonResult ->
      resultToPlan jsonResult

resultToPlan ::
  JsonResult (Plan.Planned scope param) a ->
  Plan.Plan scope param a
resultToPlan jsonResult =
  case jsonResult of
    UseRef planned ->
      Plan.use planned
    PureResult value ->
      pure value
    ApplyResult functionResult argResult ->
      resultToPlan functionResult <*> resultToPlan argResult

argToPlan ::
  Arg (Plan.Planned scope param) param a ->
  Plan.Plan scope param a
argToPlan arg =
  case arg of
    RootParam ->
      Plan.askParam
    RefField accessor _ planned ->
      Plan.use (fmap accessor planned)

--
-- Execution of the compiled form
--

-- | Runs the compiled, single-query form of the plan for one parameter.
executeJsonPlan ::
  O.MonadOrville m =>
  (forall ref. JsonPlan ref param result) ->
  param ->
  m result
executeJsonPlan plan planParam = do
  results <- executeJsonPlanList plan [planParam]
  case results of
    [one] -> pure one
    _ -> liftIO . throwIO $ ResultRowCountMismatch 1 (length results)

{- | Runs the compiled form of the plan for many parameters in one SQL query.
  Results are returned in parameter order. Failures are thrown as
  'JsonPlanError' exceptions, mirroring how native plan execution throws
  'Plan.AssertionFailed' and 'Marshall.MarshallError'.
-}
executeJsonPlanList ::
  O.MonadOrville m =>
  (forall ref. JsonPlan ref param result) ->
  [param] ->
  m [result]
executeJsonPlanList plan params =
  case NEL.nonEmpty params of
    Nothing ->
      pure []
    Just someParams -> do
      let
        query = compileJsonPlan plan someParams
        expectedCount = NEL.length someParams
      jsonTexts <- Exec.executeAndDecode Exec.SelectQuery query resultTextMarshaller
      if length jsonTexts == expectedCount
        then liftIO $ traverse (decodeBoundaryText (planDecoder plan)) jsonTexts
        else liftIO . throwIO $ ResultRowCountMismatch expectedCount (length jsonTexts)

-- | Renders the SQL that 'executeJsonPlanList' would run, for inspection.
compiledSqlText ::
  (forall ref. JsonPlan ref param result) ->
  NEL.NonEmpty param ->
  String
compiledSqlText plan params =
  BS8.unpack . RawSql.toExampleBytes $ compileJsonPlan plan params

--
-- Profunctor wrapper
--

{- | A complete, executable query: a compilable plan together with pure pre-
  and post-processing at its client-side ends. Unlike values flowing between
  a plan's steps, the parameter is a Haskell value when the query is built
  and the result is a Haskell value after the single query returns, so
  arbitrary functions may be applied at both ends without being serialized.
  That makes 'JsonQuery' a lawful 'Profunctor.Profunctor' even though the
  plan language itself has no @fmap@. Keeping the mappings outside the plan
  also keeps them away from references: a mapped value can never be bound and
  projected server-side, so the compiled and native interpretations cannot
  disagree.
-}
data JsonQuery param result where
  JsonQuery ::
    (param -> planParam) ->
    (forall ref. JsonPlan ref planParam planResult) ->
    (planResult -> result) ->
    JsonQuery param result

instance Profunctor.Profunctor JsonQuery where
  dimap f g (JsonQuery pre plan post) =
    JsonQuery (pre . f) plan (g . post)

-- | Wraps a plan as a query with no pre- or post-processing.
jsonQuery ::
  (forall ref. JsonPlan ref param result) ->
  JsonQuery param result
jsonQuery plan =
  JsonQuery id plan id

{- | Embeds a query into Orville's native plan language: the pre-processing
  maps onto 'Plan.focusParam' and the post-processing onto @fmap@.
-}
jsonQueryToPlan ::
  JsonQuery param result ->
  Plan.Plan scope param result
jsonQueryToPlan (JsonQuery pre plan post) =
  fmap post (Plan.focusParam pre (toPlan plan))

-- | Runs the compiled, single-query form of the query for one parameter.
executeJsonQuery ::
  O.MonadOrville m =>
  JsonQuery param result ->
  param ->
  m result
executeJsonQuery (JsonQuery pre plan post) queryParam =
  fmap post (executeJsonPlan plan (pre queryParam))

-- | Runs the compiled form of the query for many parameters in one SQL query.
executeJsonQueryList ::
  O.MonadOrville m =>
  JsonQuery param result ->
  [param] ->
  m [result]
executeJsonQueryList (JsonQuery pre plan post) queryParams =
  fmap (fmap post) (executeJsonPlanList plan (fmap pre queryParams))

-- | Renders the SQL that 'executeJsonQueryList' would run, for inspection.
jsonQuerySqlText ::
  JsonQuery param result ->
  NEL.NonEmpty param ->
  String
jsonQuerySqlText (JsonQuery pre plan _) queryParams =
  compiledSqlText plan (fmap pre queryParams)

--
-- Compilation to SQL
--

-- | The compile-time interpretation of a reference: the index of the CTE
--   holding the referenced step's boundary values.
newtype CteRef a = CteRef
  { cteRefIndex :: Int
  }

raw :: String -> RawSql.RawSql
raw =
  RawSql.fromString

cteName :: Int -> RawSql.RawSql
cteName index =
  raw ("jp" <> show index)

{- | Compiles a plan node into a list of (name, body) CTEs. Every CTE has the
  shape @(i, v)@: @i@ is the key of the row the value belongs to and @v@ is
  the jsonb boundary value for that row at this node. Each node preserves the
  set of @i@ values of its root CTE exactly, so later nodes can join earlier
  ones on @i@. At the top level the root CTE is the parameter CTE and @i@ is
  the parameter's position; inside a 'findAllEach' sub-plan the root CTE is
  the element CTE and @i@ is a synthetic per-element key. Takes the index of
  the node's root CTE and the next free index; returns the CTEs the node
  adds, the index of the CTE holding the node's result, and the next free
  index.
-}
compileNode ::
  JsonPlan CteRef param result ->
  Int ->
  Int ->
  ([(RawSql.RawSql, RawSql.RawSql)], Int, Int)
compileNode plan rootIndex nextIndex =
  case plan of
    FindOne tableDef fieldDef arg ->
      ([(cteName nextIndex, findOneCteBody tableDef fieldDef rootIndex arg)], nextIndex, nextIndex + 1)
    FindMaybeOne tableDef fieldDef arg ->
      ([(cteName nextIndex, findOneCteBody tableDef fieldDef rootIndex arg)], nextIndex, nextIndex + 1)
    FindAll tableDef fieldDef arg ->
      let
        body = findAllCteBody tableDef (fieldMatchesArg fieldDef rootIndex arg) (argSourceCte rootIndex arg)
      in
        ([(cteName nextIndex, body)], nextIndex, nextIndex + 1)
    FindAllWhere tableDef fieldDef cond arg ->
      let
        whereSql =
          fieldMatchesArg fieldDef rootIndex arg
            <> raw " AND (" <> RawSql.toRawSql cond <> RawSql.rightParen
        body = findAllCteBody tableDef whereSql (argSourceCte rootIndex arg)
      in
        ([(cteName nextIndex, body)], nextIndex, nextIndex + 1)
    SelectWhere tableDef cond ->
      let
        whereSql = RawSql.leftParen <> RawSql.toRawSql cond <> RawSql.rightParen
        body = findAllCteBody tableDef whereSql (cteName rootIndex)
      in
        ([(cteName nextIndex, body)], nextIndex, nextIndex + 1)
    FindAllEach tableDef fieldDef arg innerPlan ->
      let
        sourceCte = argSourceCte rootIndex arg
        elementIndex = nextIndex
        elementBody =
          raw "SELECT " <> sourceCte <> raw ".i AS outer_i, row_number() OVER () AS i, "
            <> entityJsonExpr tableDef
            <> raw " AS v FROM " <> sourceCte
            <> raw " JOIN " <> RawSql.toRawSql (Schema.tableName tableDef)
            <> raw " t ON " <> fieldMatchesArg fieldDef rootIndex arg
        (innerCtes, innerOut, afterInner) =
          compileNode (innerPlan (CteRef elementIndex)) elementIndex (elementIndex + 1)
        aggBody =
          raw "SELECT " <> sourceCte <> raw ".i, coalesce((SELECT jsonb_agg(innerVals.v ORDER BY innerVals.i) FROM "
            <> cteName elementIndex <> raw " elemRows JOIN "
            <> cteName innerOut <> raw " innerVals ON innerVals.i = elemRows.i WHERE elemRows.outer_i = "
            <> sourceCte <> raw ".i), '[]'::jsonb) AS v FROM " <> sourceCte
        ctes =
          (cteName elementIndex, elementBody)
            : innerCtes <> [(cteName afterInner, aggBody)]
      in
        (ctes, afterInner, afterInner + 1)
    Bind step continue ->
      let
        (stepCtes, stepOut, afterStep) = compileNode step rootIndex nextIndex
        (bodyCtes, bodyOut, afterBody) = compileNode (continue (CteRef stepOut)) rootIndex afterStep
      in
        (stepCtes <> bodyCtes, bodyOut, afterBody)
    Use ref ->
      ([], cteRefIndex ref, nextIndex)
    Result jsonResult ->
      ([(cteName nextIndex, resultCteBody rootIndex (resultLeafRefs jsonResult))], nextIndex, nextIndex + 1)

-- | The shared CTE body for the list-producing steps: the matched rows become
--   a jsonb array boundary value, empty when nothing matches.
findAllCteBody ::
  Schema.TableDefinition key writeEntity readEntity ->
  RawSql.RawSql ->
  RawSql.RawSql ->
  RawSql.RawSql
findAllCteBody tableDef whereSql sourceCte =
  raw "SELECT " <> sourceCte <> raw ".i, (SELECT coalesce(jsonb_agg("
    <> entityJsonExpr tableDef
    <> raw "), '[]'::jsonb) FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
    <> raw " t WHERE " <> whereSql
    <> raw ") AS v FROM " <> sourceCte

-- | The CTE indexes referenced by a result assembly, in spine order. The
--   positions match the keys used by the assembly's boundary object.
resultLeafRefs :: JsonResult CteRef a -> [Int]
resultLeafRefs jsonResult =
  case jsonResult of
    UseRef ref ->
      [cteRefIndex ref]
    PureResult _ ->
      []
    ApplyResult functionResult argResult ->
      resultLeafRefs functionResult <> resultLeafRefs argResult

{- | The CTE body for a result assembly: the referenced CTEs joined on the row
  index, built into one jsonb object with positional keys. An assembly of
  only pure values has a null boundary and keeps the row indexes from the
  parameter CTE.
-}
resultCteBody :: Int -> [Int] -> RawSql.RawSql
resultCteBody rootIndex leafIndexes =
  case leafIndexes of
    [] ->
      raw "SELECT " <> cteName rootIndex <> raw ".i, 'null'::jsonb AS v FROM " <> cteName rootIndex
    firstIndex : restIndexes ->
      let
        alias position = raw ("r" <> show (position :: Int))
        keyedValue position =
          RawSql.stringLiteral (BS8.pack (resultKeyName position))
            <> RawSql.commaSpace
            <> alias position <> raw ".v"
        joinClause (position, cteIndex) =
          raw " JOIN " <> cteName cteIndex <> raw " " <> alias position
            <> raw " ON " <> alias position <> raw ".i = r0.i"
        keyedValues =
          RawSql.intercalate
            RawSql.commaSpace
            (fmap keyedValue [0 .. length leafIndexes - 1])
      in
        raw "SELECT r0.i, jsonb_build_object(" <> keyedValues <> raw ") AS v FROM "
          <> cteName firstIndex <> raw " r0"
          <> foldMap joinClause (zip [1 ..] restIndexes)

resultKeyName :: Int -> String
resultKeyName position =
  "r" <> show position

-- | The shared CTE body for the single-row lookups: a missing row becomes a
--   jsonb null boundary value.
findOneCteBody ::
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
findOneCteBody tableDef fieldDef rootIndex arg =
  raw "SELECT " <> argSourceCte rootIndex arg <> raw ".i, coalesce((SELECT "
    <> entityJsonExpr tableDef
    <> raw " FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
    <> raw " t WHERE " <> fieldMatchesArg fieldDef rootIndex arg
    <> raw " LIMIT 1), 'null'::jsonb) AS v FROM " <> argSourceCte rootIndex arg

-- | The CTE an argument's value is read from, which is also the source of the
--   @i@ values for the step consuming the argument.
argSourceCte :: Int -> Arg CteRef param a -> RawSql.RawSql
argSourceCte rootIndex arg =
  case arg of
    RootParam ->
      cteName rootIndex
    RefField _ _ ref ->
      cteName (cteRefIndex ref)

-- | The jsonb expression holding an argument's boundary value, relative to
--   the argument's source CTE.
argJsonbExpr :: Int -> Arg CteRef param a -> RawSql.RawSql
argJsonbExpr rootIndex arg =
  case arg of
    RootParam ->
      cteName rootIndex <> raw ".v"
    RefField _ fieldDef ref ->
      cteName (cteRefIndex ref) <> raw ".v -> "
        <> RawSql.stringLiteral (fieldColumnBytes fieldDef)

{- | Builds the where condition matching a table field against an argument.
  The boundary scalar is a jsonb string holding the value's text rendering,
  so it is extracted with @#>> '{}'@ and cast back to the field's SQL type to
  keep the comparison (and any index) native.
-}
fieldMatchesArg ::
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
fieldMatchesArg fieldDef rootIndex arg =
  raw "t." <> RawSql.toRawSql (Marshall.fieldColumnName fieldDef)
    <> raw " = (((" <> argJsonbExpr rootIndex arg <> raw ") #>> '{}')::"
    <> RawSql.toRawSql (Marshall.sqlTypeExpr (Marshall.fieldType fieldDef))
    <> raw ")"

{- | Builds the jsonb_build_object expression for a table row, with every
  column cast to text so the client can replay the strings through the
  table's marshaller.
-}
entityJsonExpr ::
  Schema.TableDefinition key writeEntity readEntity ->
  RawSql.RawSql
entityJsonExpr tableDef =
  let
    fieldPair name =
      RawSql.stringLiteral name
        <> RawSql.commaSpace
        <> raw "t."
        <> RawSql.identifier name
        <> raw "::text"
  in
    raw "jsonb_build_object("
      <> RawSql.intercalate RawSql.commaSpace (fmap fieldPair (tableColumnNames tableDef))
      <> raw ")"

tableColumnNames ::
  Schema.TableDefinition key writeEntity readEntity ->
  [BS8.ByteString]
tableColumnNames tableDef =
  Marshall.foldMarshallerFields
    (Marshall.unannotatedSqlMarshaller (Schema.tableMarshaller tableDef))
    []
    ( Marshall.collectFromField
        Marshall.IncludeReadOnlyColumns
        (\_ fieldDef -> fieldColumnBytes fieldDef)
    )

fieldColumnBytes :: Marshall.FieldDefinition nullability a -> BS8.ByteString
fieldColumnBytes =
  Marshall.fieldNameToByteString . Marshall.fieldName

compileJsonPlan ::
  (forall ref. JsonPlan ref param result) ->
  NEL.NonEmpty param ->
  RawSql.RawSql
compileJsonPlan plan params =
  let
    rootCte = cteName 0

    mbEncoder = rootParamEncoder plan

    paramJsonb planParam =
      case mbEncoder of
        Just encoder ->
          raw "to_jsonb(" <> RawSql.parameter (encoder planParam) <> raw "::text)"
        Nothing ->
          raw "'null'::jsonb"

    valuesRow index planParam =
      RawSql.leftParen
        <> RawSql.intDecLiteral index
        <> RawSql.commaSpace
        <> paramJsonb planParam
        <> RawSql.rightParen

    valuesRows =
      RawSql.intercalate
        RawSql.commaSpace
        (zipWith valuesRow [0 ..] (NEL.toList params))

    (steps, outIndex, _) = compileNode plan 0 1

    renderStep (name, body) =
      name <> raw " AS (" <> body <> RawSql.rightParen

    cteList =
      (rootCte <> raw " (i, v) AS (VALUES " <> valuesRows <> RawSql.rightParen)
        : fmap renderStep steps
  in
    raw "WITH "
      <> RawSql.intercalate RawSql.commaSpace cteList
      <> raw " SELECT ("
      <> cteName outIndex
      <> raw ".v)::text AS v FROM "
      <> cteName outIndex
      <> raw " ORDER BY "
      <> cteName outIndex
      <> raw ".i"

{- | Finds the encoder for the plan's input parameter from the first step that
  consumes it. A plan that never consumes the parameter needs no encoding;
  its root CTE carries only the row indexes.
-}
rootParamEncoder ::
  JsonPlan CteRef param result ->
  Maybe (param -> SqlValue.SqlValue)
rootParamEncoder plan =
  case plan of
    FindOne _ fieldDef arg ->
      argParamEncoder fieldDef arg
    FindMaybeOne _ fieldDef arg ->
      argParamEncoder fieldDef arg
    FindAll _ fieldDef arg ->
      argParamEncoder fieldDef arg
    FindAllWhere _ fieldDef _ arg ->
      argParamEncoder fieldDef arg
    SelectWhere _ _ ->
      Nothing
    FindAllEach _ fieldDef arg _ ->
      argParamEncoder fieldDef arg
    Bind step continue ->
      case rootParamEncoder step of
        Just encoder -> Just encoder
        Nothing -> rootParamEncoder (continue (CteRef 0))
    Use _ ->
      Nothing
    Result _ ->
      Nothing

argParamEncoder ::
  Marshall.FieldDefinition nullability a ->
  Arg CteRef param a ->
  Maybe (param -> SqlValue.SqlValue)
argParamEncoder fieldDef arg =
  case arg of
    RootParam ->
      Just (Marshall.fieldValueToSqlValue fieldDef)
    RefField _ _ _ ->
      Nothing

--
-- Errors
--

-- | An error raised while executing the compiled form of a plan.
data JsonPlanError
  = -- | The query's result column could not be parsed as JSON at all.
    ServerReturnedInvalidJson String
  | -- | A jsonb boundary value did not decode to the expected Haskell value.
    BoundaryDecodeFailed DecodeError
  | -- | The compiled query returned a different number of rows than there
    --   were input parameters. The compiler guarantees these counts match,
    --   so this indicates a bug in the compiler itself.
    ResultRowCountMismatch Int Int
  deriving Show

instance Exception JsonPlanError where
  displayException =
    renderJsonPlanError

renderJsonPlanError :: JsonPlanError -> String
renderJsonPlanError err =
  case err of
    ServerReturnedInvalidJson msg ->
      "jsonplan: server returned invalid JSON: " <> msg
    BoundaryDecodeFailed decodeError ->
      "jsonplan: " <> renderDecodeError decodeError
    ResultRowCountMismatch expected actual ->
      "jsonplan: compiled query returned "
        <> show actual
        <> " rows for "
        <> show expected
        <> " parameters; this is a bug in the jsonplan compiler"

-- | Why a jsonb boundary value could not be decoded.
data DecodeError
  = -- | A 'findOne' step matched no row.
    NoRowMatched
  | ExpectedJsonArray
  | ExpectedJsonObject
  | MissingResultKey String
  | -- | An entity object held an unexpected number of rows.
    UnexpectedEntityCount Int
  | -- | The table's marshaller rejected the replayed column values.
    EntityMarshallError Marshall.MarshallError
  deriving Show

renderDecodeError :: DecodeError -> String
renderDecodeError err =
  case err of
    NoRowMatched ->
      "findOne: no row matched the argument"
    ExpectedJsonArray ->
      "findAll: expected a JSON array"
    ExpectedJsonObject ->
      "expected a JSON object"
    MissingResultKey key ->
      "result: missing key " <> key
    UnexpectedEntityCount count ->
      "expected exactly one decoded row, got " <> show count
    EntityMarshallError marshallError ->
      Marshall.renderMarshallError
        ErrorDetailLevel.maximalErrorDetailLevel
        marshallError

--
-- Decoding results
--

{- | Decodes one jsonb boundary value back into a Haskell value. Doubles as
  the decode-time interpretation of a reference. Runs in 'IO' because the
  underlying 'Marshall.marshallResultFromSql' does.
-}
newtype JsonDecoder a = JsonDecoder
  { runJsonDecoder :: Aeson.Value -> IO (Either DecodeError a)
  }

resultTextMarshaller :: Marshall.AnnotatedSqlMarshaller T.Text T.Text
resultTextMarshaller =
  Marshall.annotateSqlMarshallerEmptyAnnotation $
    Marshall.marshallField id (Marshall.unboundedTextField "v")

decodeBoundaryText :: JsonDecoder a -> T.Text -> IO a
decodeBoundaryText decoder jsonText =
  case Aeson.eitherDecodeStrict (Enc.encodeUtf8 jsonText) of
    Left err ->
      throwIO (ServerReturnedInvalidJson err)
    Right value -> do
      decoded <- runJsonDecoder decoder value
      case decoded of
        Left err -> throwIO (BoundaryDecodeFailed err)
        Right decodedValue -> pure decodedValue

planDecoder :: JsonPlan JsonDecoder param result -> JsonDecoder result
planDecoder plan =
  case plan of
    FindOne tableDef _ _ ->
      JsonDecoder $ \value ->
        case value of
          Aeson.Null ->
            pure (Left NoRowMatched)
          _ ->
            runJsonDecoder (entityDecoder tableDef) value
    FindMaybeOne tableDef _ _ ->
      JsonDecoder $ \value ->
        case value of
          Aeson.Null ->
            pure (Right Nothing)
          _ -> do
            decoded <- runJsonDecoder (entityDecoder tableDef) value
            pure (fmap Just decoded)
    FindAll tableDef _ _ ->
      entityArrayDecoder tableDef
    FindAllWhere tableDef _ _ _ ->
      entityArrayDecoder tableDef
    SelectWhere tableDef _ ->
      entityArrayDecoder tableDef
    FindAllEach tableDef _ _ innerPlan ->
      let
        innerDecoder = planDecoder (innerPlan (entityDecoder tableDef))
      in
        JsonDecoder $ \value ->
          case value of
            Aeson.Array elements ->
              decodeElements innerDecoder (Vector.toList elements)
            _ ->
              pure (Left ExpectedJsonArray)
    Bind step continue ->
      planDecoder (continue (planDecoder step))
    Use decoder ->
      decoder
    Result jsonResult ->
      resultDecoder jsonResult

decodeElements :: JsonDecoder a -> [Aeson.Value] -> IO (Either DecodeError [a])
decodeElements decoder elements =
  case elements of
    [] ->
      pure (Right [])
    element : rest -> do
      decoded <- runJsonDecoder decoder element
      case decoded of
        Left err ->
          pure (Left err)
        Right value -> do
          decodedRest <- decodeElements decoder rest
          pure (fmap (value :) decodedRest)

entityArrayDecoder ::
  Schema.TableDefinition key writeEntity readEntity ->
  JsonDecoder [readEntity]
entityArrayDecoder tableDef =
  JsonDecoder $ \value ->
    case value of
      Aeson.Array elements ->
        case traverse asObject (Vector.toList elements) of
          Left err -> pure (Left err)
          Right objects -> decodeEntityObjects tableDef objects
      _ ->
        pure (Left ExpectedJsonArray)

resultDecoder :: JsonResult JsonDecoder a -> JsonDecoder a
resultDecoder jsonResult =
  JsonDecoder $ \value ->
    let
      decodeFromObject obj = do
        decoded <- decodeResultSpine jsonResult obj 0
        pure (fmap fst decoded)
    in
      case value of
        Aeson.Object obj -> decodeFromObject obj
        Aeson.Null -> decodeFromObject AesonKeyMap.empty
        _ -> pure (Left ExpectedJsonObject)

{- | Decodes a result assembly against its boundary object, assigning keys to
  references in spine order, matching how the assembly was compiled.
-}
decodeResultSpine ::
  JsonResult JsonDecoder a ->
  Aeson.Object ->
  Int ->
  IO (Either DecodeError (a, Int))
decodeResultSpine jsonResult obj keyIndex =
  case jsonResult of
    PureResult value ->
      pure (Right (value, keyIndex))
    UseRef decoder ->
      case AesonKeyMap.lookup (AesonKey.fromString (resultKeyName keyIndex)) obj of
        Nothing ->
          pure (Left (MissingResultKey (resultKeyName keyIndex)))
        Just el -> do
          decoded <- runJsonDecoder decoder el
          pure (fmap (\value -> (value, keyIndex + 1)) decoded)
    ApplyResult functionResult argResult -> do
      decodedFunction <- decodeResultSpine functionResult obj keyIndex
      case decodedFunction of
        Left err ->
          pure (Left err)
        Right (functionValue, nextKeyIndex) -> do
          decodedArg <- decodeResultSpine argResult obj nextKeyIndex
          pure $
            case decodedArg of
              Left err -> Left err
              Right (argValue, finalKeyIndex) -> Right (functionValue argValue, finalKeyIndex)

-- | Decodes a jsonb object built by the compiled query for a table's row.
entityDecoder ::
  Schema.TableDefinition key writeEntity readEntity ->
  JsonDecoder readEntity
entityDecoder tableDef =
  JsonDecoder $ \value ->
    case value of
      Aeson.Object obj -> do
        decoded <- decodeEntityObjects tableDef [obj]
        pure $
          case decoded of
            Left err -> Left err
            Right [entity] -> Right entity
            Right entities -> Left (UnexpectedEntityCount (length entities))
      _ ->
        pure (Left ExpectedJsonObject)

asObject :: Aeson.Value -> Either DecodeError Aeson.Object
asObject value =
  case value of
    Aeson.Object obj -> Right obj
    _ -> Left ExpectedJsonObject

{- | The heart of the @::text@ trick: each jsonb entity object holds every
  column's text-mode rendering (or JSON null). Replaying those strings
  through a fake libpq result lets the table's unmodified 'SqlMarshaller'
  decode them, 'Marshall.sqlTypeFromSql' legs and all.
-}
decodeEntityObjects ::
  Schema.TableDefinition key writeEntity readEntity ->
  [Aeson.Object] ->
  IO (Either DecodeError [readEntity])
decodeEntityObjects tableDef objects =
  let
    columnNames = tableColumnNames tableDef

    columnValue obj name =
      case AesonKeyMap.lookup (AesonKey.fromText (Enc.decodeUtf8 name)) obj of
        Just (Aeson.String textValue) -> SqlValue.fromText textValue
        _ -> SqlValue.sqlNull

    fakeResult =
      Exec.mkFakeLibPQResult
        columnNames
        (fmap (\obj -> fmap (columnValue obj) columnNames) objects)
  in do
    marshalled <-
      Marshall.marshallResultFromSql
        ErrorDetailLevel.maximalErrorDetailLevel
        (Schema.tableMarshaller tableDef)
        fakeResult
    pure $
      case marshalled of
        Left err -> Left (EntityMarshallError err)
        Right entities -> Right entities
