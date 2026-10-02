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

  Every value crossing to the client is carried in a jsonb envelope. Entity
  rows cross as composite text leaves: an anonymous @ROW@ of the row's columns
  rendered to text, so each field's string is produced by its type's own
  output function and is byte-identical to what libpq's text mode would
  deliver. Decoding splits the composite and replays the strings through the
  table's existing 'Marshall.SqlMarshaller' via 'Exec.mkFakeLibPQResult'.
  Between steps, found rows additionally stay typed server-side, so field
  projections compile to native field selection.
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
  , FlatRow
  , flat
  , refCol
  , aggCol
  , Aggregation
  , aggregation
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
  , checkCompositeTextLaw
  , JsonDecoder (..)
  , planDecoder
  ) where

import Prelude hiding ((>>), (>>=))

import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as MIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import qualified Data.List.NonEmpty as NEL
import qualified Data.Maybe as Maybe
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
  Flat ::
    FlatRow ref param a ->
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

{- | Assembles a flat, report-shaped result row from scalar columns of bound
  references and server-side aggregates, via the 'Applicative' instance. A
  plan ending in a flat row compiles to a query returning ordinary SQL
  columns: no jsonb envelope and no text casts, so the values are decoded
  from the real libpq result by an ordinary 'Marshall.SqlMarshaller'.
-}
data FlatRow ref param a where
  PureFlat :: a -> FlatRow ref param a
  ApFlat ::
    FlatRow ref param (a -> b) ->
    FlatRow ref param a ->
    FlatRow ref param b
  RefCol ::
    (b -> a) ->
    Marshall.FieldDefinition nullability a ->
    ref b ->
    FlatRow ref param a
  AggCol ::
    Ord k =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability k ->
    Arg ref param k ->
    Aggregation readEntity a ->
    FlatRow ref param a

instance Functor (FlatRow ref param) where
  fmap f =
    ApFlat (PureFlat f)

instance Applicative (FlatRow ref param) where
  pure = PureFlat
  (<*>) = ApFlat

{- | A reduction of related rows to one value, carrying both interpretations:
  a Haskell fold for native execution and a SQL aggregate expression (over
  the table alias @t@) for compiled execution. The caller is trusted to keep
  the two in agreement, including any ordering the fold depends on, and the
  SQL expression must never produce NULL. The aggregate's SQL type governs
  how the compiled value is decoded.
-}
data Aggregation entity a = Aggregation
  { aggregationFold :: [entity] -> a
  , aggregationSql :: RawSql.RawSql
  , aggregationType :: Marshall.SqlType a
  }

aggregation ::
  ([entity] -> a) ->
  RawSql.RawSql ->
  Marshall.SqlType a ->
  Aggregation entity a
aggregation =
  Aggregation

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

{- | Produces a flat row as the plan's result. A flat row must be the plan's
  final node: binding it and referencing the binding is not supported by the
  compiled interpretation.
-}
flat :: FlatRow ref param a -> JsonPlan ref param a
flat =
  Flat

-- | A scalar column of a previously bound row, for use in a flat row.
refCol ::
  (b -> a) ->
  Marshall.FieldDefinition nullability a ->
  ref b ->
  FlatRow ref param a
refCol =
  RefCol

{- | An aggregate over the rows whose field matches the argument, reduced to
  one value, for use in a flat row.
-}
aggCol ::
  Ord k =>
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability k ->
  Arg ref param k ->
  Aggregation readEntity a ->
  FlatRow ref param a
aggCol =
  AggCol

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
  the field whose column name gives the native field selection for compiled
  execution. The caller is trusted to keep the two in agreement, the same
  contract 'Marshall.marshallField' already relies on.
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
    Flat flatRow ->
      flatRowToPlan flatRow

flatRowToPlan ::
  FlatRow (Plan.Planned scope param) param a ->
  Plan.Plan scope param a
flatRowToPlan flatRow =
  case flatRow of
    PureFlat value ->
      pure value
    ApFlat functionRow argRow ->
      flatRowToPlan functionRow <*> flatRowToPlan argRow
    RefCol accessor _ planned ->
      Plan.use (fmap accessor planned)
    AggCol tableDef fieldDef arg agg ->
      fmap (aggregationFold agg) $
        Plan.chain (argToPlan arg) (Plan.findAll tableDef fieldDef)

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
    _ -> MIO.liftIO . Exception.throwIO $ ResultRowCountMismatch 1 (length results)

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
      query <-
        case compileJsonPlan plan someParams of
          Left err -> MIO.liftIO (Exception.throwIO err)
          Right compiled -> pure compiled
      let
        expectedCount = NEL.length someParams
      results <-
        case terminalFlatRow plan of
          Just flatRow ->
            Exec.executeAndDecode
              Exec.SelectQuery
              query
              (Marshall.annotateSqlMarshallerEmptyAnnotation (flatRowMarshaller flatRow))
          Nothing -> do
            jsonTexts <- Exec.executeAndDecode Exec.SelectQuery query resultTextMarshaller
            MIO.liftIO $ traverse (decodeBoundaryText (planDecoder plan)) jsonTexts
      if length results == expectedCount
        then pure results
        else MIO.liftIO . Exception.throwIO $ ResultRowCountMismatch expectedCount (length results)

-- | Renders the SQL that 'executeJsonPlanList' would run, for inspection.
compiledSqlText ::
  (forall ref. JsonPlan ref param result) ->
  NEL.NonEmpty param ->
  Either JsonPlanError String
compiledSqlText plan params =
  fmap (BS8.unpack . RawSql.toExampleBytes) (compileJsonPlan plan params)

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
  Either JsonPlanError String
jsonQuerySqlText (JsonQuery pre plan _) queryParams =
  compiledSqlText plan (fmap pre queryParams)

{- | Checks the composite-text law for a field against the database: wrapping
  a value of the field's type in a composite row and parsing the row's text
  rendering must recover libpq's text-mode output byte-identically, and both
  texts must decode to the same Haskell value. This is the invariant compiled
  plan execution rests on. PostgreSQL builds composite text from each type's
  own output function, so the law is expected to hold universally; the check
  guards the composite parser and any exotic type's output quirks.
-}
checkCompositeTextLaw ::
  (O.MonadOrville m, Eq a) =>
  Marshall.FieldDefinition nullability a ->
  a ->
  m (Either String ())
checkCompositeTextLaw fieldDef value = do
  let
    typedValue =
      RawSql.leftParen
        <> RawSql.parameter (Marshall.fieldValueToSqlValue fieldDef value)
        <> raw "::"
        <> fieldCastExpr fieldDef
        <> RawSql.rightParen
    query =
      raw "SELECT "
        <> typedValue
        <> raw " AS wire, (ROW("
        <> typedValue
        <> raw "))::text AS rendered"
  probeRows <- Exec.executeAndDecode Exec.SelectQuery query wireProbeMarshaller
  pure $
    case probeRows of
      [(wireText, renderedText)] ->
        case parseCompositeText renderedText of
          Left problem ->
            Left ("parsing the rendered composite failed: " <> problem)
          Right [Just fieldText] ->
            if fieldText /= wireText
              then
                Left $
                  "wire text " <> show wireText
                    <> " differs from the composite field " <> show fieldText
              else
                let
                  decodeText probeText =
                    Marshall.sqlTypeFromSql
                      (Marshall.fieldType fieldDef)
                      (SqlValue.fromText probeText)
                in
                  case (decodeText wireText, decodeText fieldText) of
                    (Right fromWire, Right fromField) ->
                      if fromWire == fromField
                        then Right ()
                        else Left "decoding the composite field differs from decoding the wire text"
                    (Left err, _) ->
                      Left ("decoding the wire text failed: " <> err)
                    (_, Left err) ->
                      Left ("decoding the composite field failed: " <> err)
          Right [Nothing] ->
            Left "the rendered composite field was NULL"
          Right fields ->
            Left ("expected one composite field, got " <> show (length fields))
      _ ->
        Left "expected exactly one probe row"

wireProbeMarshaller :: Marshall.AnnotatedSqlMarshaller (T.Text, T.Text) (T.Text, T.Text)
wireProbeMarshaller =
  Marshall.annotateSqlMarshallerEmptyAnnotation $
    (,)
      <$> Marshall.marshallReadOnlyField (Marshall.unboundedTextField "wire")
      <*> Marshall.marshallReadOnlyField (Marshall.unboundedTextField "rendered")

--
-- Compilation to SQL
--

-- | The compile-time interpretation of a reference: the CTE holding the
--   referenced step's values.
newtype CteRef a = CteRef
  { cteRefOut :: CteOut
  }

-- | A compiled node's output CTE: its index and the shape of its value columns.
data CteOut = CteOut
  { cteOutIndex :: Int
  , cteOutShape :: CteShape
  }

{- | The column shape of a CTE. An entity CTE carries the found row twice:
  typed (@v@, the table's row type, so later steps project fields natively)
  and rendered (@b@, the row's composite text, for crossing to the client),
  both NULL when no row was found. Every other CTE carries one jsonb
  boundary value @v@.
-}
data CteShape
  = EntityCte
  | JsonbCte

cteRefIndex :: CteRef a -> Int
cteRefIndex =
  cteOutIndex . cteRefOut

{- | The jsonb expression holding a CTE's client-bound value, relative to the
  given alias. An entity CTE's composite text becomes a jsonb string, or SQL
  NULL for a missing row, which jsonb contexts turn into a JSON null.
-}
cteBoundaryExpr :: CteShape -> RawSql.RawSql -> RawSql.RawSql
cteBoundaryExpr shape alias =
  case shape of
    EntityCte ->
      raw "to_jsonb(" <> alias <> raw ".b)"
    JsonbCte ->
      alias <> raw ".v"

raw :: String -> RawSql.RawSql
raw =
  RawSql.fromString

cteName :: Int -> RawSql.RawSql
cteName index =
  raw ("jp" <> show index)

{- | Compiles a plan node into a list of (name, body) CTEs. Every CTE keys its
  rows by @i@, the key of the row the values belong to, and carries value
  columns per its 'CteShape'. Each node preserves the set of @i@ values of
  its root CTE exactly, so later nodes can join earlier ones on @i@. At the
  top level the root CTE is the parameter CTE and @i@ is the parameter's
  position; inside a 'findAllEach' sub-plan the root CTE is the element CTE
  and @i@ is a synthetic per-element key. Takes the index of the node's root
  CTE and the next free index; returns the CTEs the node adds, the CTE
  holding the node's result, and the next free index.
-}
compileNode ::
  JsonPlan CteRef param result ->
  Int ->
  Int ->
  ([(RawSql.RawSql, RawSql.RawSql)], CteOut, Int)
compileNode plan rootIndex nextIndex =
  case plan of
    FindOne tableDef fieldDef arg ->
      ( [(cteName nextIndex, findOneCteBody tableDef fieldDef rootIndex arg)]
      , CteOut nextIndex EntityCte
      , nextIndex + 1
      )
    FindMaybeOne tableDef fieldDef arg ->
      ( [(cteName nextIndex, findMaybeOneCteBody tableDef fieldDef rootIndex arg)]
      , CteOut nextIndex JsonbCte
      , nextIndex + 1
      )
    FindAll tableDef fieldDef arg ->
      let
        body = findAllCteBody tableDef (fieldMatchesArg fieldDef rootIndex arg) (argSourceCte rootIndex arg)
      in
        ([(cteName nextIndex, body)], CteOut nextIndex JsonbCte, nextIndex + 1)
    FindAllWhere tableDef fieldDef cond arg ->
      let
        whereSql =
          fieldMatchesArg fieldDef rootIndex arg
            <> raw " AND (" <> RawSql.toRawSql cond <> RawSql.rightParen
        body = findAllCteBody tableDef whereSql (argSourceCte rootIndex arg)
      in
        ([(cteName nextIndex, body)], CteOut nextIndex JsonbCte, nextIndex + 1)
    SelectWhere tableDef cond ->
      let
        whereSql = RawSql.leftParen <> RawSql.toRawSql cond <> RawSql.rightParen
        body = findAllCteBody tableDef whereSql (cteName rootIndex)
      in
        ([(cteName nextIndex, body)], CteOut nextIndex JsonbCte, nextIndex + 1)
    FindAllEach tableDef fieldDef arg innerPlan ->
      let
        sourceCte = argSourceCte rootIndex arg
        elementIndex = nextIndex
        elementBody =
          raw "SELECT " <> sourceCte <> raw ".i AS outer_i, row_number() OVER () AS i, t AS v, "
            <> rowTextExpr tableDef
            <> raw " AS b FROM " <> sourceCte
            <> raw " JOIN " <> RawSql.toRawSql (Schema.tableName tableDef)
            <> raw " t ON " <> fieldMatchesArg fieldDef rootIndex arg
        (innerCtes, innerOut, afterInner) =
          compileNode (innerPlan (CteRef (CteOut elementIndex EntityCte))) elementIndex (elementIndex + 1)
        aggBody =
          raw "SELECT " <> sourceCte <> raw ".i, coalesce((SELECT jsonb_agg("
            <> cteBoundaryExpr (cteOutShape innerOut) (raw "innerVals")
            <> raw " ORDER BY innerVals.i) FROM "
            <> cteName elementIndex <> raw " elemRows JOIN "
            <> cteName (cteOutIndex innerOut) <> raw " innerVals ON innerVals.i = elemRows.i WHERE elemRows.outer_i = "
            <> sourceCte <> raw ".i), '[]'::jsonb) AS v FROM " <> sourceCte
        ctes =
          (cteName elementIndex, elementBody)
            : innerCtes <> [(cteName afterInner, aggBody)]
      in
        (ctes, CteOut afterInner JsonbCte, afterInner + 1)
    Bind step continue ->
      let
        (stepCtes, stepOut, afterStep) = compileNode step rootIndex nextIndex
        (bodyCtes, bodyOut, afterBody) = compileNode (continue (CteRef stepOut)) rootIndex afterStep
      in
        (stepCtes <> bodyCtes, bodyOut, afterBody)
    Use ref ->
      ([], cteRefOut ref, nextIndex)
    Result jsonResult ->
      ( [(cteName nextIndex, resultCteBody rootIndex (resultLeafRefs jsonResult))]
      , CteOut nextIndex JsonbCte
      , nextIndex + 1
      )
    Flat flatRow ->
      ( [(cteName nextIndex, flatCteBody rootIndex flatRow)]
      , CteOut nextIndex JsonbCte
      , nextIndex + 1
      )

{- | The CTE body for a flat row: the root CTE's keys joined with every
  referenced CTE, projecting each column as an ordinary SQL value under a
  positional alias. Scalar columns are native field selections on entity
  rows; aggregate columns become correlated aggregate subqueries over their
  table.
-}
flatCteBody :: Int -> FlatRow CteRef param a -> RawSql.RawSql
flatCteBody rootIndex flatRow =
  let
    (columnSqls, _) = flatColumnSqls rootIndex flatRow 0
    joinIndexes =
      List.nub (List.filter (/= rootIndex) (flatRowRefIndexes rootIndex flatRow))
    joinClause cteIndex =
      raw " JOIN " <> cteName cteIndex <> raw " ON "
        <> cteName cteIndex <> raw ".i = " <> cteName rootIndex <> raw ".i"
    selectColumns =
      (cteName rootIndex <> raw ".i") : columnSqls
  in
    raw "SELECT "
      <> RawSql.intercalate RawSql.commaSpace selectColumns
      <> raw " FROM " <> cteName rootIndex
      <> foldMap joinClause joinIndexes

flatColumnSqls :: Int -> FlatRow CteRef param a -> Int -> ([RawSql.RawSql], Int)
flatColumnSqls rootIndex flatRow columnIndex =
  case flatRow of
    PureFlat _ ->
      ([], columnIndex)
    ApFlat functionRow argRow ->
      let
        (functionSqls, afterFunction) = flatColumnSqls rootIndex functionRow columnIndex
        (argSqls, afterArg) = flatColumnSqls rootIndex argRow afterFunction
      in
        (functionSqls <> argSqls, afterArg)
    RefCol _ fieldDef ref ->
      let
        columnSql =
          RawSql.leftParen <> cteName (cteRefIndex ref) <> raw ".v)."
            <> RawSql.identifier (fieldColumnBytes fieldDef)
            <> raw " AS " <> raw (flatColumnName columnIndex)
      in
        ([columnSql], columnIndex + 1)
    AggCol tableDef fieldDef arg agg ->
      let
        columnSql =
          raw "(SELECT " <> aggregationSql agg <> raw " FROM "
            <> RawSql.toRawSql (Schema.tableName tableDef)
            <> raw " t WHERE " <> fieldMatchesArg fieldDef rootIndex arg
            <> raw ") AS " <> raw (flatColumnName columnIndex)
      in
        ([columnSql], columnIndex + 1)

flatRowRefIndexes :: Int -> FlatRow CteRef param a -> [Int]
flatRowRefIndexes rootIndex flatRow =
  case flatRow of
    PureFlat _ ->
      []
    ApFlat functionRow argRow ->
      flatRowRefIndexes rootIndex functionRow <> flatRowRefIndexes rootIndex argRow
    RefCol _ _ ref ->
      [cteRefIndex ref]
    AggCol _ _ arg _ ->
      case arg of
        RootParam -> [rootIndex]
        RefField _ _ ref -> [cteRefIndex ref]

flatColumnName :: Int -> String
flatColumnName columnIndex =
  "c" <> show columnIndex

-- | Finds the flat row a plan ends in, if any, by walking its bindings.
terminalFlatRow ::
  JsonPlan CteRef param result ->
  Maybe (FlatRow CteRef param result)
terminalFlatRow plan =
  case plan of
    Flat flatRow ->
      Just flatRow
    Bind _ continue ->
      terminalFlatRow (continue (CteRef (CteOut 0 JsonbCte)))
    _ ->
      Nothing

-- | The shared CTE body for the list-producing steps: the matched rows become
--   a jsonb array of composite text leaves, empty when nothing matches.
findAllCteBody ::
  Schema.TableDefinition key writeEntity readEntity ->
  RawSql.RawSql ->
  RawSql.RawSql ->
  RawSql.RawSql
findAllCteBody tableDef whereSql sourceCte =
  raw "SELECT " <> sourceCte <> raw ".i, (SELECT coalesce(jsonb_agg("
    <> rowTextExpr tableDef
    <> raw "), '[]'::jsonb) FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
    <> raw " t WHERE " <> whereSql
    <> raw ") AS v FROM " <> sourceCte

-- | The CTEs referenced by a result assembly, in spine order. The positions
--   match the keys used by the assembly's boundary object.
resultLeafRefs :: JsonResult CteRef a -> [CteOut]
resultLeafRefs jsonResult =
  case jsonResult of
    UseRef ref ->
      [cteRefOut ref]
    PureResult _ ->
      []
    ApplyResult functionResult argResult ->
      resultLeafRefs functionResult <> resultLeafRefs argResult

{- | The CTE body for a result assembly: the referenced CTEs joined on the row
  index, their boundary values built into one jsonb object with positional
  keys. An assembly of only pure values has a null boundary and keeps the row
  indexes from the parameter CTE.
-}
resultCteBody :: Int -> [CteOut] -> RawSql.RawSql
resultCteBody rootIndex leaves =
  case leaves of
    [] ->
      raw "SELECT " <> cteName rootIndex <> raw ".i, 'null'::jsonb AS v FROM " <> cteName rootIndex
    firstLeaf : restLeaves ->
      let
        alias position = raw ("r" <> show (position :: Int))
        keyedValue (position, leaf) =
          RawSql.stringLiteral (BS8.pack (resultKeyName position))
            <> RawSql.commaSpace
            <> cteBoundaryExpr (cteOutShape leaf) (alias position)
        joinClause (position, leaf) =
          raw " JOIN " <> cteName (cteOutIndex leaf) <> raw " " <> alias position
            <> raw " ON " <> alias position <> raw ".i = r0.i"
        keyedValues =
          fmap keyedValue (zip [0 ..] leaves)
      in
        raw "SELECT r0.i, " <> jsonbBuildObjectChunked keyedValues <> raw " AS v FROM "
          <> cteName (cteOutIndex firstLeaf) <> raw " r0"
          <> foldMap joinClause (zip [1 ..] restLeaves)

resultKeyName :: Int -> String
resultKeyName position =
  "r" <> show position

{- | The CTE body for 'findOne': an entity CTE holding the matched row typed
  and as composite text, both NULL when no row matches.
-}
findOneCteBody ::
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
findOneCteBody tableDef fieldDef rootIndex arg =
  let
    matchedRow selected =
      raw "(SELECT " <> selected
        <> raw " FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
        <> raw " t WHERE " <> fieldMatchesArg fieldDef rootIndex arg
        <> raw " LIMIT 1)"
  in
    raw "SELECT " <> argSourceCte rootIndex arg <> raw ".i, "
      <> matchedRow (raw "t") <> raw " AS v, "
      <> matchedRow (rowTextExpr tableDef) <> raw " AS b FROM "
      <> argSourceCte rootIndex arg

-- | The CTE body for 'findMaybeOne': a missing row becomes a jsonb null
--   boundary value directly, since the result is never projected.
findMaybeOneCteBody ::
  Schema.TableDefinition key writeEntity readEntity ->
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
findMaybeOneCteBody tableDef fieldDef rootIndex arg =
  raw "SELECT " <> argSourceCte rootIndex arg <> raw ".i, coalesce((SELECT to_jsonb("
    <> rowTextExpr tableDef
    <> raw ") FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
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

{- | The SQL expression for an argument's value, relative to the argument's
  source CTE. The root parameter crosses as a jsonb string holding the
  value's text rendering, so it is extracted and cast back to the matched
  field's SQL type to keep the comparison (and any index) native; a projected
  field is native field selection on the referenced entity row.
-}
argValueExpr ::
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
argValueExpr fieldDef rootIndex arg =
  case arg of
    RootParam ->
      raw "((" <> cteName rootIndex <> raw ".v #>> '{}')::"
        <> fieldCastExpr fieldDef
        <> RawSql.rightParen
    RefField _ projectedField ref ->
      RawSql.leftParen <> cteName (cteRefIndex ref) <> raw ".v)."
        <> RawSql.identifier (fieldColumnBytes projectedField)

-- | Builds the where condition matching a table field against an argument.
fieldMatchesArg ::
  Marshall.FieldDefinition nullability a ->
  Int ->
  Arg CteRef param a ->
  RawSql.RawSql
fieldMatchesArg fieldDef rootIndex arg =
  raw "t." <> RawSql.toRawSql (Marshall.fieldColumnName fieldDef)
    <> raw " = " <> argValueExpr fieldDef rootIndex arg

{- | The type to use when casting a boundary value back to a field's type.
  Uses the reference data type when the field's type has one, so that
  pseudo-types like SERIAL cast to their underlying type.
-}
fieldCastExpr :: Marshall.FieldDefinition nullability a -> RawSql.RawSql
fieldCastExpr fieldDef =
  let
    sqlType = Marshall.fieldType fieldDef
  in
    RawSql.toRawSql $
      Maybe.fromMaybe
        (Marshall.sqlTypeExpr sqlType)
        (Marshall.sqlTypeReferenceExpr sqlType)

{- | The composite text expression for a table row: an anonymous @ROW@ of the
  marshaller's columns in marshaller order (so the client can map the fields
  back positionally), rendered to text by PostgreSQL's composite output
  routine. Each field's string is produced by its type's own output function,
  which is exactly what libpq's text mode delivers, so after composite
  unquoting the strings replay through the table's 'Marshall.SqlMarshaller'
  unchanged. Row constructors are not subject to PostgreSQL's 100-argument
  limit on function calls, so no chunking is needed.
-}
rowTextExpr ::
  Schema.TableDefinition key writeEntity readEntity ->
  RawSql.RawSql
rowTextExpr tableDef =
  raw "ROW("
    <> RawSql.intercalate
      RawSql.commaSpace
      (fmap tableColumnSelectSql (tableColumns tableDef))
    <> raw ")::text"

{- | Builds a jsonb object from rendered key-value pairs, in chunks of
  jsonb_build_object calls concatenated with @||@ to stay under PostgreSQL's
  100-argument limit on function calls.
-}
jsonbBuildObjectChunked :: [RawSql.RawSql] -> RawSql.RawSql
jsonbBuildObjectChunked keyedValues =
  let
    buildObject chunk =
      raw "jsonb_build_object("
        <> RawSql.intercalate RawSql.commaSpace chunk
        <> raw ")"
  in
    RawSql.leftParen
      <> RawSql.intercalate (raw " || ") (fmap buildObject (pairChunks keyedValues))
      <> RawSql.rightParen

pairChunks :: [RawSql.RawSql] -> [[RawSql.RawSql]]
pairChunks keyedValues =
  case keyedValues of
    [] -> []
    _ -> List.take 25 keyedValues : pairChunks (List.drop 25 keyedValues)

{- | One column of a table's composite text leaf: the column name the decoder
  replays it as, and the expression that selects it, relative to the table
  alias @t@. The list order of 'tableColumns' is the composite's field order.
-}
data TableColumn = TableColumn
  { tableColumnName :: BS8.ByteString
  , tableColumnSelectSql :: RawSql.RawSql
  }

tableColumns ::
  Schema.TableDefinition key writeEntity readEntity ->
  [TableColumn]
tableColumns tableDef =
  Marshall.foldMarshallerFields
    (Marshall.unannotatedSqlMarshaller (Schema.tableMarshaller tableDef))
    []
    collectTableColumn

collectTableColumn ::
  Marshall.MarshallerField writeEntity ->
  [TableColumn] ->
  [TableColumn]
collectTableColumn entry columns =
  case entry of
    Marshall.Natural _ fieldDef _ ->
      TableColumn
        (fieldColumnBytes fieldDef)
        (raw "t." <> RawSql.identifier (fieldColumnBytes fieldDef))
        : columns
    Marshall.Synthetic synthField ->
      TableColumn
        (Marshall.fieldNameToByteString (Marshall.syntheticFieldName synthField))
        ( RawSql.leftParen
            <> RawSql.toRawSql (Marshall.syntheticFieldExpression synthField)
            <> RawSql.rightParen
        )
        : columns

fieldColumnBytes :: Marshall.FieldDefinition nullability a -> BS8.ByteString
fieldColumnBytes =
  Marshall.fieldNameToByteString . Marshall.fieldName

compileJsonPlan ::
  (forall ref. JsonPlan ref param result) ->
  NEL.NonEmpty param ->
  Either JsonPlanError RawSql.RawSql
compileJsonPlan plan params =
  case planShapeError plan of
    Just err ->
      Left err
    Nothing ->
      Right (compileCheckedJsonPlan plan params)

orElseError :: Maybe e -> Maybe e -> Maybe e
orElseError firstError secondError =
  case firstError of
    Just err -> Just err
    Nothing -> secondError

{- | The boundary shape a reference points at. Field projections only have a
  server-side meaning against a single entity row.
-}
data BoundaryShape
  = EntityShape
  | NonEntityShape String

-- | The shape-checking interpretation of a reference.
newtype ShapeRef a = ShapeRef
  { shapeRefShape :: BoundaryShape
  }

{- | Finds the first field projection applied to a reference without an
  entity boundary, if any.
-}
planShapeError :: JsonPlan ShapeRef param result -> Maybe JsonPlanError
planShapeError plan =
  case plan of
    FindOne _ _ arg ->
      argShapeError arg
    FindMaybeOne _ _ arg ->
      argShapeError arg
    FindAll _ _ arg ->
      argShapeError arg
    FindAllWhere _ _ _ arg ->
      argShapeError arg
    SelectWhere _ _ ->
      Nothing
    FindAllEach _ _ arg innerPlan ->
      orElseError
        (argShapeError arg)
        (planShapeError (innerPlan (ShapeRef EntityShape)))
    Bind step continue ->
      orElseError
        (planShapeError step)
        (planShapeError (continue (ShapeRef (boundaryShapeOf step))))
    Use _ ->
      Nothing
    Result _ ->
      Nothing
    Flat flatRow ->
      flatRowShapeError flatRow

boundaryShapeOf :: JsonPlan ShapeRef param result -> BoundaryShape
boundaryShapeOf plan =
  case plan of
    FindOne _ _ _ ->
      EntityShape
    FindMaybeOne _ _ _ ->
      NonEntityShape "findMaybeOne"
    FindAll _ _ _ ->
      NonEntityShape "findAll"
    FindAllWhere _ _ _ _ ->
      NonEntityShape "findAllWhere"
    SelectWhere _ _ ->
      NonEntityShape "selectWhere"
    FindAllEach _ _ _ _ ->
      NonEntityShape "findAllEach"
    Bind step continue ->
      boundaryShapeOf (continue (ShapeRef (boundaryShapeOf step)))
    Use ref ->
      shapeRefShape ref
    Result _ ->
      NonEntityShape "result"
    Flat _ ->
      NonEntityShape "flat"

argShapeError :: Arg ShapeRef param a -> Maybe JsonPlanError
argShapeError arg =
  case arg of
    RootParam ->
      Nothing
    RefField _ fieldDef ref ->
      case shapeRefShape ref of
        EntityShape ->
          Nothing
        NonEntityShape stepKind ->
          Just (RefFieldOnNonEntity stepKind (BS8.unpack (fieldColumnBytes fieldDef)))

flatRowShapeError :: FlatRow ShapeRef param a -> Maybe JsonPlanError
flatRowShapeError flatRow =
  case flatRow of
    PureFlat _ ->
      Nothing
    ApFlat functionRow argRow ->
      orElseError (flatRowShapeError functionRow) (flatRowShapeError argRow)
    RefCol _ fieldDef ref ->
      case shapeRefShape ref of
        EntityShape ->
          Nothing
        NonEntityShape stepKind ->
          Just (RefFieldOnNonEntity stepKind (BS8.unpack (fieldColumnBytes fieldDef)))
    AggCol _ _ arg _ ->
      argShapeError arg

compileCheckedJsonPlan ::
  (forall ref. JsonPlan ref param result) ->
  NEL.NonEmpty param ->
  RawSql.RawSql
compileCheckedJsonPlan plan params =
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

    (steps, outCte, _) = compileNode plan 0 1

    outName = cteName (cteOutIndex outCte)

    renderStep (name, body) =
      name <> raw " AS (" <> body <> RawSql.rightParen

    cteList =
      (rootCte <> raw " (i, v) AS (VALUES " <> valuesRows <> RawSql.rightParen)
        : fmap renderStep steps

    finalSelect =
      case terminalFlatRow plan of
        Just _ ->
          raw " SELECT * FROM " <> outName
        Nothing ->
          raw " SELECT coalesce("
            <> cteBoundaryExpr (cteOutShape outCte) outName
            <> raw ", 'null'::jsonb)::text AS v FROM "
            <> outName
  in
    raw "WITH "
      <> RawSql.intercalate RawSql.commaSpace cteList
      <> finalSelect
      <> raw " ORDER BY "
      <> outName
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
        Nothing -> rootParamEncoder (continue (CteRef (CteOut 0 JsonbCte)))
    Use _ ->
      Nothing
    Result _ ->
      Nothing
    Flat flatRow ->
      flatRowParamEncoder flatRow

flatRowParamEncoder ::
  FlatRow CteRef param a ->
  Maybe (param -> SqlValue.SqlValue)
flatRowParamEncoder flatRow =
  case flatRow of
    PureFlat _ ->
      Nothing
    ApFlat functionRow argRow ->
      case flatRowParamEncoder functionRow of
        Just encoder -> Just encoder
        Nothing -> flatRowParamEncoder argRow
    RefCol _ _ _ ->
      Nothing
    AggCol _ fieldDef arg _ ->
      argParamEncoder fieldDef arg

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
  | -- | A field (column name) was projected from a reference whose boundary
    --   (named by its step kind) is not a single entity row, so the
    --   projection has no server-side meaning and the plan cannot be
    --   compiled.
    RefFieldOnNonEntity String String
  deriving Show

instance Exception.Exception JsonPlanError where
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
    RefFieldOnNonEntity stepKind columnName ->
      "jsonplan: refField on column "
        <> columnName
        <> " references a "
        <> stepKind
        <> " result, which is not a single entity row; only findOne rows and findAllEach element rows can be projected. The plan can still be executed natively via toPlan"

-- | Why a jsonb boundary value could not be decoded.
data DecodeError
  = -- | A 'findOne' step matched no row.
    NoRowMatched
  | -- | A boundary was not the expected JSON array; carries the step kind.
    ExpectedJsonArray String
  | -- | A boundary was not the expected JSON object; carries the context.
    ExpectedJsonObject String
  | -- | A boundary was not the expected JSON string; carries the context.
    ExpectedJsonString String
  | -- | A composite text rendering could not be parsed; carries the problem.
    MalformedCompositeText String
  | -- | A composite held a different number of fields than the table's
    --   marshaller expects: expected count, actual count.
    CompositeArityMismatch Int Int
  | MissingResultKey String
  | -- | An entity object held an unexpected number of rows.
    UnexpectedEntityCount Int
  | -- | A flat row was bound mid-plan instead of being the plan's final node.
    FlatRowNotTerminal
  | -- | The table's marshaller rejected the replayed column values.
    EntityMarshallError Marshall.MarshallError
  deriving Show

renderDecodeError :: DecodeError -> String
renderDecodeError err =
  case err of
    NoRowMatched ->
      "findOne: no row matched the argument"
    ExpectedJsonArray stepKind ->
      stepKind <> ": expected a JSON array"
    ExpectedJsonObject context ->
      context <> ": expected a JSON object"
    ExpectedJsonString context ->
      context <> ": expected a JSON string"
    MalformedCompositeText problem ->
      "malformed composite text: " <> problem
    CompositeArityMismatch expected actual ->
      "composite row has " <> show actual <> " fields, expected " <> show expected
    MissingResultKey key ->
      "result: missing key " <> key
    UnexpectedEntityCount count ->
      "expected exactly one decoded row, got " <> show count
    FlatRowNotTerminal ->
      "flat: a flat row must be the plan's final node"
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
      Exception.throwIO (ServerReturnedInvalidJson err)
    Right value -> do
      decoded <- runJsonDecoder decoder value
      case decoded of
        Left err -> Exception.throwIO (BoundaryDecodeFailed err)
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
      entityArrayDecoder "findAll" tableDef
    FindAllWhere tableDef _ _ _ ->
      entityArrayDecoder "findAllWhere" tableDef
    SelectWhere tableDef _ ->
      entityArrayDecoder "selectWhere" tableDef
    FindAllEach tableDef _ _ innerPlan ->
      let
        innerDecoder = planDecoder (innerPlan (entityDecoder tableDef))
      in
        JsonDecoder $ \value ->
          case value of
            Aeson.Array elements ->
              decodeElements innerDecoder (Vector.toList elements)
            _ ->
              pure (Left (ExpectedJsonArray "findAllEach"))
    Bind step continue ->
      planDecoder (continue (planDecoder step))
    Use decoder ->
      decoder
    Result jsonResult ->
      resultDecoder jsonResult
    Flat _ ->
      JsonDecoder $ \_ ->
        pure (Left FlatRowNotTerminal)

{- | Builds the marshaller that decodes a flat row from the compiled query's
  ordinary result columns, assigning positional column names in spine order,
  matching how the flat row was compiled.
-}
flatRowMarshaller :: FlatRow ref param a -> Marshall.SqlMarshaller () a
flatRowMarshaller flatRow =
  let
    (marshaller, _) = flatRowMarshallerFrom flatRow 0
  in
    marshaller

flatRowMarshallerFrom ::
  FlatRow ref param a ->
  Int ->
  (Marshall.SqlMarshaller () a, Int)
flatRowMarshallerFrom flatRow columnIndex =
  case flatRow of
    PureFlat value ->
      (pure value, columnIndex)
    ApFlat functionRow argRow ->
      let
        (functionMarshaller, afterFunction) = flatRowMarshallerFrom functionRow columnIndex
        (argMarshaller, afterArg) = flatRowMarshallerFrom argRow afterFunction
      in
        (functionMarshaller <*> argMarshaller, afterArg)
    RefCol _ fieldDef _ ->
      (flatColumnMarshaller columnIndex (Marshall.fieldType fieldDef), columnIndex + 1)
    AggCol _ _ _ agg ->
      (flatColumnMarshaller columnIndex (aggregationType agg), columnIndex + 1)

flatColumnMarshaller :: Int -> Marshall.SqlType a -> Marshall.SqlMarshaller () a
flatColumnMarshaller columnIndex sqlType =
  Marshall.marshallReadOnlyField
    (Marshall.fieldOfType sqlType (flatColumnName columnIndex))

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
  String ->
  Schema.TableDefinition key writeEntity readEntity ->
  JsonDecoder [readEntity]
entityArrayDecoder stepKind tableDef =
  let
    columnNames = fmap tableColumnName (tableColumns tableDef)
  in
    JsonDecoder $ \value ->
      case value of
        Aeson.Array elements ->
          case traverse (compositeFields "entity element") (Vector.toList elements) of
            Left err -> pure (Left err)
            Right rows -> decodeEntityRows tableDef columnNames rows
        _ ->
          pure (Left (ExpectedJsonArray stepKind))

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
        _ -> pure (Left (ExpectedJsonObject "result"))

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

-- | Decodes a composite text leaf built by the compiled query for a table's row.
entityDecoder ::
  Schema.TableDefinition key writeEntity readEntity ->
  JsonDecoder readEntity
entityDecoder tableDef =
  let
    columnNames = fmap tableColumnName (tableColumns tableDef)
  in
    JsonDecoder $ \value ->
      case compositeFields "entity" value of
        Left err ->
          pure (Left err)
        Right fields -> do
          decoded <- decodeEntityRows tableDef columnNames [fields]
          pure $
            case decoded of
              Left err -> Left err
              Right [entity] -> Right entity
              Right entities -> Left (UnexpectedEntityCount (length entities))

-- | Extracts a composite text leaf's fields from its jsonb boundary value.
compositeFields :: String -> Aeson.Value -> Either DecodeError [Maybe T.Text]
compositeFields context value =
  case value of
    Aeson.String compositeText ->
      case parseCompositeText compositeText of
        Left problem -> Left (MalformedCompositeText problem)
        Right fields -> Right fields
    _ ->
      Left (ExpectedJsonString context)

{- | The heart of the composite-leaf representation: each leaf holds every
  column's text-mode rendering in marshaller column order (or NULL).
  Replaying those strings through a fake libpq result lets the table's
  unmodified 'Marshall.SqlMarshaller' decode them,
  'Marshall.sqlTypeFromSql' legs and all.
-}
decodeEntityRows ::
  Schema.TableDefinition key writeEntity readEntity ->
  [BS8.ByteString] ->
  [[Maybe T.Text]] ->
  IO (Either DecodeError [readEntity])
decodeEntityRows tableDef columnNames rows =
  let
    columnCount = length columnNames

    rowValues fields =
      if length fields == columnCount
        then Right (fmap (maybe SqlValue.sqlNull SqlValue.fromText) fields)
        else Left (CompositeArityMismatch columnCount (length fields))
  in
    case traverse rowValues rows of
      Left err ->
        pure (Left err)
      Right sqlRows -> do
        marshalled <-
          Marshall.marshallResultFromSql
            ErrorDetailLevel.maximalErrorDetailLevel
            (Schema.tableMarshaller tableDef)
            (Exec.mkFakeLibPQResult columnNames sqlRows)
        pure $
          case marshalled of
            Left err -> Left (EntityMarshallError err)
            Right entities -> Right entities

{- | Parses PostgreSQL's composite text output format: a parenthesized,
  comma-separated field list where an empty field is NULL and a field
  containing special characters is double-quoted with embedded quotes and
  backslashes doubled (or backslash-escaped).
-}
parseCompositeText :: T.Text -> Either String [Maybe T.Text]
parseCompositeText compositeText =
  case T.uncons compositeText of
    Just ('(', afterOpen) ->
      parseCompositeFields afterOpen []
    _ ->
      Left "expected '(' at the start of a composite value"

parseCompositeFields :: T.Text -> [Maybe T.Text] -> Either String [Maybe T.Text]
parseCompositeFields input parsedFields =
  case parseCompositeField input of
    Left err ->
      Left err
    Right (field, rest) ->
      case T.uncons rest of
        Just (',', afterComma) ->
          parseCompositeFields afterComma (field : parsedFields)
        Just (')', afterClose)
          | T.null afterClose ->
              Right (reverse (field : parsedFields))
          | otherwise ->
              Left "unexpected input after the closing ')'"
        _ ->
          Left "expected ',' or ')' after a composite field"

parseCompositeField :: T.Text -> Either String (Maybe T.Text, T.Text)
parseCompositeField input =
  case T.uncons input of
    Just ('"', afterQuote) ->
      fmap
        (\(fieldText, rest) -> (Just fieldText, rest))
        (parseQuotedField afterQuote mempty)
    _ ->
      let
        (fieldText, rest) = T.span (\c -> c /= ',' && c /= ')') input
      in
        Right (if T.null fieldText then Nothing else Just fieldText, rest)

parseQuotedField :: T.Text -> T.Text -> Either String (T.Text, T.Text)
parseQuotedField input parsedText =
  let
    (plain, rest) = T.break (\c -> c == '"' || c == '\\') input
  in
    case T.uncons rest of
      Nothing ->
        Left "unterminated quoted composite field"
      Just (special, afterSpecial)
        | special == '\\' ->
            case T.uncons afterSpecial of
              Nothing ->
                Left "unterminated escape in a quoted composite field"
              Just (escaped, remaining) ->
                parseQuotedField remaining (parsedText <> plain <> T.singleton escaped)
        | otherwise ->
            case T.uncons afterSpecial of
              Just ('"', remaining) ->
                parseQuotedField remaining (parsedText <> plain <> T.singleton '"')
              _ ->
                Right (parsedText <> plain, afterSpecial)
