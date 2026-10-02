{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- | A scratch prototype of a restricted plan DSL that can either be embedded
  into Orville's native 'Plan.Plan' (via 'toPlan') or compiled to a single SQL
  query that chains CTEs and returns nested results as jsonb.

  Every value crossing a step boundary server-side is represented as jsonb.
  Scalar and column values are cast to @text@ before being placed into jsonb,
  so that the strings coming back are byte-identical to what libpq's text mode
  would deliver. Decoding therefore reuses the table's existing
  'Marshall.SqlMarshaller' by replaying the strings through a
  'Exec.mkFakeLibPQResult'.
-}
module Orville.JsonPlan
  ( JsonPlan (..)
  , Projection
  , fieldProjection
  , JsonDecoder (..)
  , entityDecoder
  , toPlan
  , executeJsonPlan
  , executeJsonPlanList
  , compiledSqlText
  ) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NEL
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.Vector as Vector

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.ErrorDetailLevel as ErrorDetailLevel
import qualified Orville.PostgreSQL.Execution as Exec
import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Schema as Schema

{- | The restricted plan vocabulary. Each constructor has both a native
  interpretation ('toPlan') and a jsonb/CTE compilation
  ('executeJsonPlanList'). Arbitrary Haskell functions are permitted only
  inside 'Projection', paired with the SQL expression they correspond to.
-}
data JsonPlan param result where
  FindOne ::
    (Show param, Ord param) =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability param ->
    JsonPlan param readEntity
  FindAll ::
    Ord param =>
    Schema.TableDefinition key writeEntity readEntity ->
    Marshall.FieldDefinition nullability param ->
    JsonPlan param [readEntity]
  Focus ::
    Projection a b ->
    JsonPlan b result ->
    JsonPlan a result
  Chain ::
    JsonPlan a b ->
    JsonPlan b c ->
    JsonPlan a c
  WithParam ::
    JsonDecoder param ->
    JsonPlan param result ->
    JsonPlan param (param, result)

{- | A parameter projection carrying both interpretations: a Haskell accessor
  for native execution, and the field whose column name and SQL type give the
  jsonb path and cast for compiled execution. The caller is trusted to keep
  the two in agreement, the same contract 'Marshall.marshallField' already
  relies on.
-}
data Projection a b where
  FieldProjection ::
    (a -> b) ->
    Marshall.FieldDefinition nullability b ->
    Projection a b

fieldProjection ::
  (a -> b) ->
  Marshall.FieldDefinition nullability b ->
  Projection a b
fieldProjection =
  FieldProjection

{- | Decodes one jsonb boundary value back into a Haskell value. Runs in 'IO'
  because the underlying 'Marshall.marshallResultFromSql' does.
-}
newtype JsonDecoder a = JsonDecoder
  { runJsonDecoder :: Aeson.Value -> IO (Either String a)
  }

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
            Right _ -> Left "entityDecoder: expected exactly one decoded row"
      _ ->
        pure (Left "entityDecoder: expected a JSON object")

-- | Embeds a 'JsonPlan' into Orville's native plan language.
toPlan :: JsonPlan param result -> Plan.Plan scope param result
toPlan jsonPlan =
  case jsonPlan of
    FindOne tableDef fieldDef ->
      Plan.findOne tableDef fieldDef
    FindAll tableDef fieldDef ->
      Plan.findAll tableDef fieldDef
    Focus (FieldProjection accessor _) subPlan ->
      Plan.focusParam accessor (toPlan subPlan)
    Chain firstPlan secondPlan ->
      Plan.chain (toPlan firstPlan) (toPlan secondPlan)
    WithParam _ subPlan ->
      (,) <$> Plan.askParam <*> toPlan subPlan

-- | Runs the compiled, single-query form of the plan for one parameter.
executeJsonPlan ::
  O.MonadOrville m =>
  JsonPlan param result ->
  param ->
  m result
executeJsonPlan plan param = do
  results <- executeJsonPlanList plan [param]
  case results of
    [one] -> pure one
    _ -> liftIO . throwIO . userError $ "jsonplan: expected exactly one result row"

{- | Runs the compiled form of the plan for many parameters in one SQL query.
  Results are returned in parameter order. Decoding failures are thrown as
  exceptions, mirroring how native plan execution throws 'AssertionFailed'.
-}
executeJsonPlanList ::
  O.MonadOrville m =>
  JsonPlan param result ->
  [param] ->
  m [result]
executeJsonPlanList plan params =
  case NEL.nonEmpty params of
    Nothing ->
      pure []
    Just someParams ->
      case rootParamToSql plan of
        Left err ->
          liftIO . throwIO . userError $ err
        Right encoder -> do
          let
            query = compileJsonPlan plan (fmap encoder someParams)
          jsonTexts <- Exec.executeAndDecode Exec.SelectQuery query resultTextMarshaller
          liftIO $ traverse (decodeBoundaryText (planDecoder plan)) jsonTexts

-- | Renders the SQL that 'executeJsonPlanList' would run, for inspection.
compiledSqlText ::
  JsonPlan param result ->
  NEL.NonEmpty param ->
  Either String String
compiledSqlText plan params =
  case rootParamToSql plan of
    Left err ->
      Left err
    Right encoder ->
      Right . BS8.unpack . RawSql.toExampleBytes $
        compileJsonPlan plan (fmap encoder params)

--
-- Compilation to SQL
--

raw :: String -> RawSql.RawSql
raw =
  RawSql.fromString

cteName :: Int -> RawSql.RawSql
cteName index =
  raw ("jp" <> show index)

{- | Compiles a plan node into a list of (name, body) CTEs. Every CTE has the
  shape @(i, v)@: @i@ is the input parameter's position and @v@ is the jsonb
  boundary value for that parameter at this step. Each step preserves the set
  of @i@ values exactly.
-}
compileNode ::
  JsonPlan param result ->
  RawSql.RawSql ->
  Int ->
  ([(RawSql.RawSql, RawSql.RawSql)], RawSql.RawSql, Int)
compileNode plan inputCte nextIndex =
  case plan of
    FindOne tableDef fieldDef ->
      let
        name = cteName nextIndex
        body =
          raw "SELECT " <> inputCte <> raw ".i, coalesce((SELECT "
            <> entityJsonExpr tableDef
            <> raw " FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
            <> raw " t WHERE " <> fieldMatchesBoundary fieldDef inputCte
            <> raw " LIMIT 1), 'null'::jsonb) AS v FROM " <> inputCte
      in
        ([(name, body)], name, nextIndex + 1)
    FindAll tableDef fieldDef ->
      let
        name = cteName nextIndex
        body =
          raw "SELECT " <> inputCte <> raw ".i, (SELECT coalesce(jsonb_agg("
            <> entityJsonExpr tableDef
            <> raw "), '[]'::jsonb) FROM " <> RawSql.toRawSql (Schema.tableName tableDef)
            <> raw " t WHERE " <> fieldMatchesBoundary fieldDef inputCte
            <> raw ") AS v FROM " <> inputCte
      in
        ([(name, body)], name, nextIndex + 1)
    Focus (FieldProjection _ fieldDef) subPlan ->
      let
        name = cteName nextIndex
        body =
          raw "SELECT " <> inputCte <> raw ".i, (" <> inputCte <> raw ".v -> "
            <> RawSql.stringLiteral (fieldColumnBytes fieldDef)
            <> raw ") AS v FROM " <> inputCte
        (subSteps, subOut, afterSub) = compileNode subPlan name (nextIndex + 1)
      in
        ((name, body) : subSteps, subOut, afterSub)
    Chain firstPlan secondPlan ->
      let
        (firstSteps, firstOut, afterFirst) = compileNode firstPlan inputCte nextIndex
        (secondSteps, secondOut, afterSecond) = compileNode secondPlan firstOut afterFirst
      in
        (firstSteps <> secondSteps, secondOut, afterSecond)
    WithParam _ subPlan ->
      let
        (subSteps, subOut, afterSub) = compileNode subPlan inputCte nextIndex
        name = cteName afterSub
        body =
          raw "SELECT " <> inputCte <> raw ".i, jsonb_build_object('fst', "
            <> inputCte <> raw ".v, 'snd', " <> subOut <> raw ".v) AS v FROM "
            <> inputCte <> raw " JOIN " <> subOut <> raw " ON "
            <> subOut <> raw ".i = " <> inputCte <> raw ".i"
      in
        (subSteps <> [(name, body)], name, afterSub + 1)

{- | Builds the where condition matching a table field against the incoming
  boundary value. The boundary scalar is a jsonb string holding the value's
  text rendering, so it is extracted with @#>> '{}'@ and cast back to the
  field's SQL type to keep the comparison (and any index) native.
-}
fieldMatchesBoundary ::
  Marshall.FieldDefinition nullability a ->
  RawSql.RawSql ->
  RawSql.RawSql
fieldMatchesBoundary fieldDef inputCte =
  raw "t." <> RawSql.toRawSql (Marshall.fieldColumnName fieldDef)
    <> raw " = ((" <> inputCte <> raw ".v #>> '{}')::"
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
  JsonPlan param result ->
  NEL.NonEmpty SqlValue.SqlValue ->
  RawSql.RawSql
compileJsonPlan plan sqlParams =
  let
    rootCte = cteName 0

    valuesRow index sqlValue =
      RawSql.leftParen
        <> RawSql.intDecLiteral index
        <> raw ", to_jsonb("
        <> RawSql.parameter sqlValue
        <> raw "::text)"
        <> RawSql.rightParen

    valuesRows =
      RawSql.intercalate
        RawSql.commaSpace
        (zipWith valuesRow [0 ..] (NEL.toList sqlParams))

    (steps, outCte, _) = compileNode plan rootCte 1

    renderStep (name, body) =
      name <> raw " AS (" <> body <> RawSql.rightParen

    cteList =
      (rootCte <> raw " (i, v) AS (VALUES " <> valuesRows <> RawSql.rightParen)
        : fmap renderStep steps
  in
    raw "WITH "
      <> RawSql.intercalate RawSql.commaSpace cteList
      <> raw " SELECT ("
      <> outCte
      <> raw ".v)::text AS v FROM "
      <> outCte
      <> raw " ORDER BY "
      <> outCte
      <> raw ".i"

{- | Finds the encoder for the plan's root parameter by walking to the
  leftmost operation. A plan whose first step is a bare projection has no SQL
  rendering for its parameter type; that is a prototype limitation.
-}
rootParamToSql ::
  JsonPlan param result ->
  Either String (param -> SqlValue.SqlValue)
rootParamToSql plan =
  case plan of
    FindOne _ fieldDef ->
      Right (Marshall.fieldValueToSqlValue fieldDef)
    FindAll _ fieldDef ->
      Right (Marshall.fieldValueToSqlValue fieldDef)
    Chain firstPlan _ ->
      rootParamToSql firstPlan
    WithParam _ subPlan ->
      rootParamToSql subPlan
    Focus _ _ ->
      Left "jsonplan: cannot encode the root parameter of a plan that begins with a projection"

--
-- Decoding results
--

resultTextMarshaller :: Marshall.AnnotatedSqlMarshaller T.Text T.Text
resultTextMarshaller =
  Marshall.annotateSqlMarshallerEmptyAnnotation $
    Marshall.marshallField id (Marshall.unboundedTextField "v")

decodeBoundaryText :: JsonDecoder a -> T.Text -> IO a
decodeBoundaryText decoder jsonText =
  case Aeson.eitherDecodeStrict (Enc.encodeUtf8 jsonText) of
    Left err ->
      throwIO . userError $ "jsonplan: server returned invalid JSON: " <> err
    Right value -> do
      decoded <- runJsonDecoder decoder value
      case decoded of
        Left err -> throwIO . userError $ "jsonplan: " <> err
        Right result -> pure result

planDecoder :: JsonPlan param result -> JsonDecoder result
planDecoder plan =
  case plan of
    FindOne tableDef _ ->
      JsonDecoder $ \value ->
        case value of
          Aeson.Null ->
            pure (Left "FindOne: no row matched the parameter")
          _ ->
            runJsonDecoder (entityDecoder tableDef) value
    FindAll tableDef _ ->
      JsonDecoder $ \value ->
        case value of
          Aeson.Array elements ->
            case traverse asObject (Vector.toList elements) of
              Left err -> pure (Left err)
              Right objects -> decodeEntityObjects tableDef objects
          _ ->
            pure (Left "FindAll: expected a JSON array")
    Focus _ subPlan ->
      planDecoder subPlan
    Chain _ secondPlan ->
      planDecoder secondPlan
    WithParam paramDecoder subPlan ->
      JsonDecoder $ \value ->
        case value of
          Aeson.Object obj ->
            let
              lookupKey key =
                case AesonKeyMap.lookup (AesonKey.fromString key) obj of
                  Nothing -> Left ("WithParam: missing key " <> key)
                  Just el -> Right el
            in
              case (,) <$> lookupKey "fst" <*> lookupKey "snd" of
                Left err ->
                  pure (Left err)
                Right (fstValue, sndValue) -> do
                  decodedParam <- runJsonDecoder paramDecoder fstValue
                  decodedResult <- runJsonDecoder (planDecoder subPlan) sndValue
                  pure ((,) <$> decodedParam <*> decodedResult)
          _ ->
            pure (Left "WithParam: expected a JSON object")

asObject :: Aeson.Value -> Either String Aeson.Object
asObject value =
  case value of
    Aeson.Object obj -> Right obj
    _ -> Left "expected a JSON object"

{- | The heart of the @::text@ trick: each jsonb entity object holds every
  column's text-mode rendering (or JSON null). Replaying those strings
  through a fake libpq result lets the table's unmodified 'SqlMarshaller'
  decode them, 'Marshall.sqlTypeFromSql' legs and all.
-}
decodeEntityObjects ::
  Schema.TableDefinition key writeEntity readEntity ->
  [Aeson.Object] ->
  IO (Either String [readEntity])
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
        Left err -> Left (show err)
        Right entities -> Right entities
