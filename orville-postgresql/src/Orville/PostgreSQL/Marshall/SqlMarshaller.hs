{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

This module provides functions for constructing a mapping between Haskell data
types and SQL column schemas. The 'SqlMarshaller' that represents this mapping
can be used to serialize Haskell values both to and from SQL column sets. In
most cases, you construct a 'SqlMarshaller' as part of building your
'Orville.PostgreSQL.Schema.TableDefinition' and Orville handles the rest. In
other cases, you might use a 'SqlMarshaller' with a lower-level Orville
function. For instance, to decode the result set of a custom SQL query.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Marshall.SqlMarshaller
  ( SqlMarshaller
  , AnnotatedSqlMarshaller
  , annotateSqlMarshaller
  , annotateSqlMarshallerEmptyAnnotation
  , unannotatedSqlMarshaller
  , mapSqlMarshaller
  , MarshallerField (Natural, Synthetic)
  , marshallResultFromSql
  , marshallResultFromSqlUsingRowIdExtractor
  , RowIdentityExtractor
  , mkRowIdentityExtractor
  , marshallField
  , marshallSyntheticField
  , marshallReadOnlyField
  , marshallReadOnly
  , marshallNested
  , marshallMaybe
  , marshallPartial
  , marshallAlias
  , prefixMarshaller
  , ReadOnlyColumnOption (IncludeReadOnlyColumns, ExcludeReadOnlyColumns)
  , collectFromField
  , marshallEntityToSetClauses
  , foldMarshallerFields
  , marshallerDerivedColumns
  , marshallerTableConstraints
  , mkRowSource
  , RowSource
  , mapRowSource
  , applyRowSource
  , constRowSource
  , failRowSource
  , marshallerEquals
  , marshallerNotEquals
  , marshallerIsDistinctFrom
  , marshallerIsNotDistinctFrom
  , marshallerLessThan
  , marshallerLessThanOrEqualTo
  , marshallerGreaterThan
  , marshallerGreaterThanOrEqualTo
  , marshallerIn
  , marshallerNotIn
  )
where

import Control.Monad (join)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

import Orville.PostgreSQL.ErrorDetailLevel (ErrorDetailLevel)
import Orville.PostgreSQL.Execution.ExecutionResult (Column (Column), ExecutionResult, Row (Row))
import qualified Orville.PostgreSQL.Execution.ExecutionResult as Result
import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Marshall.AliasName (AliasName, aliasNameAsFieldName)
import Orville.PostgreSQL.Marshall.FieldDefinition (FieldDefinition, FieldName, FieldNullability (NotNullField, NullableField), asymmetricNullableField, buildAliasedFieldDefinition, fieldAliasQualifiedColumnName, fieldColumnName, fieldName, fieldNameToByteString, fieldNameToColumnName, fieldNullability, fieldTableConstraints, fieldValueFromSqlValue, nullableField, prefixField, setField)
import qualified Orville.PostgreSQL.Marshall.MarshallError as MarshallError
import qualified Orville.PostgreSQL.Marshall.SqlComparable as SqlComparable
import Orville.PostgreSQL.Marshall.SyntheticField (SyntheticField, nullableSyntheticField, prefixSyntheticField, syntheticFieldAlias, syntheticFieldExpression, syntheticFieldValueFromSqlValue)
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue
import qualified Orville.PostgreSQL.Schema.ConstraintDefinition as ConstraintDefinition

{- | An 'AnnotatedSqlMarshaller' is a 'SqlMarshaller' that contains extra
  annotations which cannot necessarily be determined from the data in the
  marshaller itself. In particular, it includes the names of fields that can be
  used to identify a row in the database when an error is encountered during
  decoding.

  Normally you will not need to interact with this type directly -- the
  @TableDefinition@ type creates it for you using the information it has about
  the primary key of the table to identify rows in decoding errors. If you are
  executing custom queries directly, you may need to annotate a raw
  'SqlMarshaller' yourself so that rows can be identified. See
  'annotateSqlMarshaller' and 'annotateSqlMarshallerEmptyAnnotation'.

@since 1.0.0.0
-}
data AnnotatedSqlMarshaller writeEntity readEntity = AnnotatedSqlMarshaller
  { rowIdFieldNames :: [FieldName]
  , unannotatedSqlMarshaller :: SqlMarshaller writeEntity readEntity
  }

{- | Creates an 'AnnotatedSqlMarshaller' that will use the given column names
  to identify rows in error messages when decoding fails. Any column names
  in the list that are not present in the result set will simply be omitted
  from the error message.

@since 1.0.0.0
-}
annotateSqlMarshaller ::
  [FieldName] ->
  SqlMarshaller writeEntity readEntity ->
  AnnotatedSqlMarshaller writeEntity readEntity
annotateSqlMarshaller =
  AnnotatedSqlMarshaller

{- | Creates an 'AnnotatedSqlMarshaller' that will identify rows in decoding
  errors by any columns. This is the equivalent of @annotateSqlMarshaller []@.

@since 1.0.0.0
-}
annotateSqlMarshallerEmptyAnnotation ::
  SqlMarshaller writeEntity readEntity ->
  AnnotatedSqlMarshaller writeEntity readEntity
annotateSqlMarshallerEmptyAnnotation =
  annotateSqlMarshaller []

{- | Applies the provided function to a 'SqlMarshaller' that has been annotated,
  preserving the annotations.

@since 1.0.0.0
-}
mapSqlMarshaller ::
  (SqlMarshaller readEntityA writeEntityA -> SqlMarshaller readEntityB writeEntityB) ->
  AnnotatedSqlMarshaller readEntityA writeEntityA ->
  AnnotatedSqlMarshaller readEntityB writeEntityB
mapSqlMarshaller f (AnnotatedSqlMarshaller rowIdFields marshaller) =
  AnnotatedSqlMarshaller rowIdFields (f marshaller)

{- | 'SqlMarshaller' is how we group the lowest-level translation of single fields
  into a higher-level marshalling of full SQL records into Haskell records.
  This is a flexible abstraction that allows us to ultimately model SQL tables
  and work with them as potentially nested Haskell records. We can then
  "marshall" the data as we want to model it in SQL and Haskell.

@since 1.0.0.0
-}
data SqlMarshaller a b where
  -- | Our representation of 'pure' in the 'Applicative' sense.
  MarshallPure :: b -> SqlMarshaller a b
  -- | Representation of application like '<*>' from 'Applicative'.
  MarshallApply :: SqlMarshaller a (b -> c) -> SqlMarshaller a b -> SqlMarshaller a c
  -- | Nest an arbitrary function; this is used when modeling a SQL table as nested Haskell records.
  MarshallNest :: (a -> b) -> SqlMarshaller b c -> SqlMarshaller a c
  -- | Marshall a SQL column using the given 'FieldDefinition'.
  MarshallField :: FieldDefinition nullability a -> SqlMarshaller a a
  -- | Marshall a SQL expression on selecting using the given 'SyntheticField'
  -- to generate selects. SyntheticFields are implicitly read-only, as they
  -- do not represent a column that can be inserted into.
  MarshallSyntheticField :: SyntheticField a -> SqlMarshaller b a
  -- | Tag a maybe-mapped marshaller so we don't map it twice.
  MarshallMaybeTag :: SqlMarshaller (Maybe a) (Maybe b) -> SqlMarshaller (Maybe a) (Maybe b)
  -- | Marshall a column with a possibility of error.
  MarshallPartial :: SqlMarshaller a (Either String b) -> SqlMarshaller a b
  -- | Marshall a column that is read-only, like auto-incrementing ids.
  MarshallReadOnly :: SqlMarshaller a b -> SqlMarshaller c b
  -- | Apply an alias to a marshaller
  MarshallAlias :: AliasName -> SqlMarshaller a b -> SqlMarshaller a b

-- | @since 1.0.0.0
instance Functor (SqlMarshaller a) where
  fmap f = MarshallApply (MarshallPure f)

-- | @since 1.0.0.0
instance Applicative (SqlMarshaller a) where
  pure = MarshallPure
  (<*>) = MarshallApply

{- | Marshallers with no natural columns will result in @null@ 'SqlValue.SqlValue' and
  'Expr.ValueExpression' as the outputs of 'SqlComparable.toComparableSqlValue' and
  'SqlComparable.referenceValueExpression', respectively.

  Both methods exclude columns marshalled with 'marshallReadOnly'.

@since 1.1.0.0
-}
instance SqlComparable.SqlComparable (SqlMarshaller writeEntity readEntity) writeEntity where
  toComparableSqlValue marshaller writeEntity =
    let
      collectSqlVals marshallerField vals =
        case marshallerField of
          Natural _ fieldDef (Just getField) ->
            SqlComparable.toComparableSqlValue fieldDef (getField writeEntity) : vals
          _ -> vals
    in
      maybe
        SqlValue.sqlNull
        SqlValue.fromRow
        (NE.nonEmpty $ foldMarshallerFields marshaller [] collectSqlVals)

  referenceValueExpression marshaller =
    let
      colValExprs =
        foldMarshallerFields marshaller [] $
          collectFromField
            ExcludeReadOnlyColumns
            ( \mbAlias fieldDef ->
                case mbAlias of
                  Nothing -> SqlComparable.referenceValueExpression fieldDef
                  Just alias ->
                    SqlComparable.referenceValueExpression $
                      buildAliasedFieldDefinition fieldDef alias
            )
    in
      maybe
        (Expr.valueExpression SqlValue.sqlNull)
        Expr.rowValueConstructor
        (NE.nonEmpty colValExprs)

{- | Returns a list of 'Expr.DerivedColumn' expressions that can be used in a
  select statement to select values from the database for the 'SqlMarshaller'
  decode.

@since 1.0.0.0
-}
marshallerDerivedColumns ::
  SqlMarshaller writeEntity readEntity ->
  [Expr.DerivedColumn]
marshallerDerivedColumns marshaller =
  let
    collectDerivedColumn ::
      MarshallerField writeEntity ->
      [Expr.DerivedColumn] ->
      [Expr.DerivedColumn]
    collectDerivedColumn entry columns =
      case entry of
        Natural mbAlias fieldDef _ ->
          let
            colName =
              case mbAlias of
                Nothing -> fieldColumnName fieldDef
                Just alias -> Expr.untrackQualified $ fieldAliasQualifiedColumnName alias fieldDef
          in
            (Expr.deriveColumn $ Expr.columnReference colName) : columns
        Synthetic synthField ->
          Expr.deriveColumnAs
            (syntheticFieldExpression synthField)
            (fieldNameToColumnName . aliasNameAsFieldName $ syntheticFieldAlias synthField)
            : columns
  in
    foldMarshallerFields marshaller [] collectDerivedColumn

{- | Returns the table constraints for all the 'FieldDefinition's used in the
  'SqlMarshaller'.

@since 1.0.0.0
-}
marshallerTableConstraints ::
  SqlMarshaller writeEntity readEntity ->
  ConstraintDefinition.TableConstraints
marshallerTableConstraints marshaller =
  let
    collectTableConstraints ::
      MarshallerField writeEntity ->
      ConstraintDefinition.TableConstraints ->
      ConstraintDefinition.TableConstraints
    collectTableConstraints entry constraints =
      case entry of
        Natural _ fieldDef _ -> constraints <> fieldTableConstraints fieldDef
        Synthetic _synthField -> constraints
  in
    foldMarshallerFields
      marshaller
      ConstraintDefinition.emptyTableConstraints
      collectTableConstraints

{- | Represents a primitive entry in a 'SqlMarshaller'. This type is used with
  'foldMarshallerFields' to provided the entry from the marshaller to the
  folding function to be incorporated in the result of the fold.

@since 1.0.0.0
-}
data MarshallerField writeEntity where
  Natural :: Maybe AliasName -> FieldDefinition nullability a -> Maybe (writeEntity -> a) -> MarshallerField writeEntity
  Synthetic :: SyntheticField a -> MarshallerField writeEntity

{- | A fold function that can be used with 'foldMarshallerFields' to collect
  a value calculated from a 'FieldDefinition' via the given function. The calculated
  value is added to the list of values being built.

  Note: Folds executed with 'collectFromField' ignore 'Synthetic' entries in
  the marshaller. You should only use 'collectFromField' in situations where
  you only care about the actual columns referenced by the marshaller.

@since 1.0.0.0
-}
collectFromField ::
  ReadOnlyColumnOption ->
  (forall nullability a. Maybe AliasName -> FieldDefinition nullability a -> result) ->
  MarshallerField entity ->
  [result] ->
  [result]
collectFromField readOnlyColumnOption fromField entry results =
  case entry of
    Natural mbAlias fieldDef (Just _) ->
      fromField mbAlias fieldDef : results
    Natural mbAlias fieldDef Nothing ->
      case readOnlyColumnOption of
        IncludeReadOnlyColumns -> fromField mbAlias fieldDef : results
        ExcludeReadOnlyColumns -> results
    Synthetic _ ->
      results

{- | Uses the field definitions in the marshaller to construct SQL expressions
  that will set columns of the field definitions to their corresponding values
  found in the Haskell @writeEntity@ value.

@since 1.0.0.0
-}
marshallEntityToSetClauses ::
  SqlMarshaller writeEntity readEntity ->
  writeEntity ->
  [Expr.SetClause]
marshallEntityToSetClauses marshaller writeEntity =
  foldMarshallerFields
    marshaller
    []
    (collectSetClauses writeEntity)

{- | An internal helper function that collects the 'Expr.SetClause's to
  update all the fields contained in a 'SqlMarshaller'

@since 1.0.0.0
-}
collectSetClauses ::
  entity ->
  MarshallerField entity ->
  [Expr.SetClause] ->
  [Expr.SetClause]
collectSetClauses entity entry clauses =
  case entry of
    Natural _ fieldDef (Just accessor) ->
      setField fieldDef (accessor entity) : clauses
    Natural _ _ Nothing ->
      clauses
    Synthetic _ ->
      clauses

{- | Specifies whether read-only fields should be included when using functions
  such as 'collectFromField'.

@since 1.0.0.0
-}
data ReadOnlyColumnOption
  = IncludeReadOnlyColumns
  | ExcludeReadOnlyColumns

{- | 'foldMarshallerFields' allows you to consume the 'FieldDefinition's that
  are contained within the 'SqlMarshaller' to process them however is
  required. This can be used to collect the names of all the fields, encode
  them to 'SqlValue.SqlValue', etc.

@since 1.0.0.0
-}
foldMarshallerFields ::
  SqlMarshaller writeEntity readEntity ->
  result ->
  (MarshallerField writeEntity -> result -> result) ->
  result
foldMarshallerFields marshaller =
  foldMarshallerFieldsPart Nothing marshaller (Just id)

{- | The internal helper function that actually implements 'foldMarshallerFields'.
  It takes with it a function that extracts the current nesting entity from
  the overall @writeEntity@ that the 'SqlMarshaller' is build on. 'MarshallNest'
  adds more nesting by composing its accessor with the one given here.

@since 1.0.0.0
-}
foldMarshallerFieldsPart ::
  Maybe AliasName ->
  SqlMarshaller entityPart readEntity ->
  Maybe (writeEntity -> entityPart) ->
  result ->
  (MarshallerField writeEntity -> result -> result) ->
  result
foldMarshallerFieldsPart mbAlias marshaller getPart currentResult addToResult =
  case marshaller of
    MarshallPure _ ->
      currentResult
    MarshallApply submarshallerA submarshallerB ->
      let
        subresultB =
          foldMarshallerFieldsPart mbAlias submarshallerB getPart currentResult addToResult
      in
        foldMarshallerFieldsPart mbAlias submarshallerA getPart subresultB addToResult
    MarshallNest nestingFunction submarshaller ->
      foldMarshallerFieldsPart mbAlias submarshaller (fmap (nestingFunction .) getPart) currentResult addToResult
    MarshallField fieldDefinition ->
      addToResult (Natural mbAlias fieldDefinition getPart) currentResult
    MarshallSyntheticField syntheticField ->
      addToResult (Synthetic syntheticField) currentResult
    MarshallMaybeTag m ->
      foldMarshallerFieldsPart mbAlias m getPart currentResult addToResult
    MarshallPartial m ->
      foldMarshallerFieldsPart mbAlias m getPart currentResult addToResult
    MarshallReadOnly m ->
      foldMarshallerFieldsPart mbAlias m Nothing currentResult addToResult
    MarshallAlias a m ->
      foldMarshallerFieldsPart (Just a) m getPart currentResult addToResult

{- | Decodes all the rows found in an execution result at once. The first row that
  fails to decode will return the 'MarshallError.MarshallErrorDetails' that
  results, otherwise all decoded rows will be returned.

  Note that this function loads all decoded rows into memory at once, so it
  should only be used with result sets that you know will fit into memory.

@since 1.0.0.0
-}
marshallResultFromSql ::
  ExecutionResult result =>
  ErrorDetailLevel ->
  AnnotatedSqlMarshaller writeEntity readEntity ->
  result ->
  IO (Either MarshallError.MarshallError [readEntity])
marshallResultFromSql errorDetailLevel marshallerWithMeta result =
  marshallResultFromSqlUsingRowIdExtractor
    errorDetailLevel
    (mkRowIdentityExtractor (rowIdFieldNames marshallerWithMeta) result)
    (unannotatedSqlMarshaller marshallerWithMeta)
    result

{- | Decodes all the rows found in a execution result at once. The first row that
  fails to decode will return the 'MarshallError.MarshallErrorDetails' that
  results, otherwise all decoded rows will be returned. If an error occurs
  while decoding a row, the 'RowIdentityExtractor' will be used to extract
  values to identify the row in the error details.

  Note that this function loads all decoded rows into memory at once, so it
  should only be used with result sets that you know will fit into memory.

@since 1.0.0.0
-}
marshallResultFromSqlUsingRowIdExtractor ::
  ExecutionResult result =>
  ErrorDetailLevel ->
  RowIdentityExtractor ->
  SqlMarshaller writeEntity readEntity ->
  result ->
  IO (Either MarshallError.MarshallError [readEntity])
marshallResultFromSqlUsingRowIdExtractor errorDetailLevel rowIdExtractor marshaller result = do
  mbMaxRow <- Result.maxRowNumber result

  case mbMaxRow of
    Nothing ->
      pure (Right [])
    Just maxRow -> do
      rowSource <- mkRowSource marshaller result
      traverseSequence (decodeRow errorDetailLevel rowSource rowIdExtractor) [Row 0 .. maxRow]

traverseSequence :: (a -> IO (Either err b)) -> [a] -> IO (Either err [b])
traverseSequence f =
  go
 where
  go as =
    case as of
      [] ->
        pure (Right [])
      a : rest -> do
        eitherB <- f a
        case eitherB of
          Left err ->
            pure (Left err)
          Right b -> do
            eitherBS <- go rest
            case eitherBS of
              Left err ->
                pure (Left err)
              Right bs ->
                pure (Right (b : bs))

{- | Attempts to decode a result set row that has already been fetched from the
  database server into a Haskell value. If the decoding fails, a
  'MarshallError.MarshallError' will be returned.

@since 1.0.0.0
-}
decodeRow ::
  ErrorDetailLevel ->
  RowSource readEntity ->
  RowIdentityExtractor ->
  Row ->
  IO (Either MarshallError.MarshallError readEntity)
decodeRow errorDetailLevel (RowSource source) (RowIdentityExtractor getRowId) row = do
  result <- source row
  case result of
    Left err -> do
      rowId <- getRowId row
      pure
        . Left
        $ MarshallError.MarshallError
          { MarshallError.marshallErrorDetailLevel = errorDetailLevel
          , MarshallError.marshallErrorRowIdentifier = rowId
          , MarshallError.marshallErrorDetails = err
          }
    Right entity ->
      pure $
        Right entity

{- | A 'RowSource' can fetch and decode rows from a database result set. Using
  a 'RowSource' gives random access to the rows in the result set, only
  attempting to decode them when they are requested by the user via 'decodeRow'.

  Note that even though the rows are not decoded into Haskell until 'decodeRow'
  is called, all the rows returned from the query are held in memory on the
  client waiting to be decoded until the 'RowSource' is garbage collected.
  As such, you can't use 'RowSource' (alone) to achieve any form of streaming
  or pagination of rows between the database server and the client.

@since 1.0.0.0
-}
newtype RowSource readEntity
  = RowSource (Row -> IO (Either MarshallError.MarshallErrorDetails readEntity))

-- | @since 1.0.0.0
instance Functor RowSource where
  fmap = mapRowSource

-- | @since 1.0.0.0
instance Applicative RowSource where
  pure = constRowSource
  (<*>) = applyRowSource

{- | Adds a function to the decoding proocess to transform the value returned
  by a 'RowSource'.

@since 1.0.0.0
-}
mapRowSource :: (a -> b) -> RowSource a -> RowSource b
mapRowSource f (RowSource decodeA) =
  RowSource $ \row -> fmap (fmap f) (decodeA row)

{- | Creates a 'RowSource' that always returns the value given, rather than
  attempting to access the result set and decoding anything.

@since 1.0.0.0
-}
constRowSource :: readEntity -> RowSource readEntity
constRowSource =
  RowSource . const . pure . Right

{- | Applies a function that will be decoded from the result set to another
  value decoded from the result set.

@since 1.0.0.0
-}
applyRowSource :: RowSource (a -> b) -> RowSource a -> RowSource b
applyRowSource (RowSource decodeAtoB) (RowSource decodeA) =
  RowSource $ \row -> do
    eitherAToB <- decodeAtoB row

    case eitherAToB of
      Left err ->
        pure (Left err)
      Right aToB -> do
        eitherA <- decodeA row
        pure (fmap aToB eitherA)

{- | Creates a 'RowSource' that will always fail to decode by returning the
  provided error. This can be used in cases where a 'RowSource' must
  be provided but it is already known at run time that decoding is impossible.
  For instance, this is used internally when a 'FieldDefinition' references
  a column that does not exist in the result set.

@since 1.0.0.0
-}
failRowSource :: MarshallError.MarshallErrorDetails -> RowSource a
failRowSource =
  RowSource . const . pure . Left

{- | Uses the 'SqlMarshaller' given to build a 'RowSource' that will decode
  from the given result set. The returned 'RowSource' can then be used to
  decode rows as desired by the user. Note that the entire result set is
  held in memory for potential decoding until the 'RowSource' is garbage
  collected.

@since 1.0.0.0
-}
mkRowSource ::
  ExecutionResult result =>
  SqlMarshaller writeEntity readEntity ->
  result ->
  IO (RowSource readEntity)
mkRowSource marshaller result = do
  columnMap <- prepareColumnMap result

  let
    mkSource :: Maybe AliasName -> SqlMarshaller a b -> RowSource b
    mkSource mbAlias marshallerPart =
      -- Note, this case statement is evaluated before the row argument is
      -- ever passed to a 'RowSource' to ensure that a single 'RowSource'
      -- operation is build and re-used when decoding many rows.
      case marshallerPart of
        MarshallPure readEntity ->
          constRowSource readEntity
        MarshallApply marshallAToB marshallA ->
          mkSource mbAlias marshallAToB <*> mkSource mbAlias marshallA
        MarshallNest _ someMarshaller ->
          mkSource mbAlias someMarshaller
        MarshallField fieldDef ->
          mkFieldNameSource
            (fieldName fieldDef)
            (fieldValueFromSqlValue fieldDef)
            columnMap
            result
        MarshallSyntheticField syntheticField ->
          mkFieldNameSource
            (aliasNameAsFieldName $ syntheticFieldAlias syntheticField)
            (syntheticFieldValueFromSqlValue syntheticField)
            columnMap
            result
        MarshallMaybeTag m ->
          mkSource mbAlias m
        MarshallPartial m ->
          let
            fieldNames =
              foldMarshallerFields m [] $ \marshallerField names ->
                case marshallerField of
                  Natural _ field _ ->
                    fieldName field : names
                  Synthetic field ->
                    aliasNameAsFieldName (syntheticFieldAlias field) : names
          in
            partialRowSource fieldNames columnMap result (mkSource mbAlias m)
        MarshallReadOnly m ->
          mkSource mbAlias m
        MarshallAlias a m ->
          mkSource (Just a) m

  pure . mkSource Nothing $ marshaller

partialRowSource ::
  ExecutionResult result =>
  [FieldName] ->
  Map.Map B8.ByteString Column ->
  result ->
  RowSource (Either String readEntity) ->
  RowSource readEntity
partialRowSource fieldNames columnMap result (RowSource f) =
  RowSource $ \row -> do
    partialResult <- f row
    case partialResult of
      Left marshallError ->
        pure $ Left marshallError
      Right (Left errorMessage) -> do
        let
          columnNames =
            map fieldNameToByteString fieldNames

          lookupValue columnName =
            case Map.lookup columnName columnMap of
              Nothing ->
                pure (columnName, SqlValue.sqlNull)
              Just columnNumber -> do
                value <- Result.getValue result row columnNumber
                pure (columnName, value)

        values <- traverse lookupValue columnNames

        pure . Left . MarshallError.DecodingError $
          MarshallError.DecodingErrorDetails
            { MarshallError.decodingErrorValues = values
            , MarshallError.decodingErrorMessage = errorMessage
            }
      Right (Right entity) ->
        pure $ Right entity

{- | Builds a 'RowSource' that will retrieve and decode the name field from
  the result.

@since 1.0.0.0
-}
mkFieldNameSource ::
  ExecutionResult result =>
  FieldName ->
  (SqlValue.SqlValue -> Either String a) ->
  Map.Map B8.ByteString Column ->
  result ->
  RowSource a
mkFieldNameSource sourceFieldName fromSqlValue columnMap result =
  case Map.lookup (fieldNameToByteString sourceFieldName) columnMap of
    Just columnNumber ->
      mkColumnRowSource sourceFieldName fromSqlValue result columnNumber
    Nothing ->
      failRowSource . MarshallError.MissingColumnError $
        MarshallError.MissingColumnErrorDetails
          { MarshallError.missingColumnName = fieldNameToByteString sourceFieldName
          , MarshallError.actualColumnNames = Map.keysSet columnMap
          }

{- | An internal helper function that finds all the column names in a result set
  and associates them with the respective column numbers for easier lookup.

@since 1.0.0.0
-}
prepareColumnMap ::
  ExecutionResult result =>
  result ->
  IO (Map.Map B8.ByteString Column)
prepareColumnMap result = do
  mbMaxColumn <- Result.maxColumnNumber result

  let
    mkNameEntry columnNumber = do
      mbColumnName <- Result.columnName result columnNumber

      pure $
        case mbColumnName of
          Just name ->
            Just (name, columnNumber)
          Nothing ->
            Nothing

  case mbMaxColumn of
    Nothing ->
      pure Map.empty
    Just maxColumn -> do
      entries <- traverse mkNameEntry [Column 0 .. maxColumn]
      pure $ Map.fromList (catMaybes entries)

{- | A internal helper function for to build a 'RowSource' that retrieves and
  decodes a single column value form the result set.

@since 1.0.0.0
-}
mkColumnRowSource ::
  ExecutionResult result =>
  FieldName ->
  (SqlValue.SqlValue -> Either String a) ->
  result ->
  Column ->
  RowSource a
mkColumnRowSource sourceFieldName fromSqlValue result column =
  RowSource $ \row -> do
    sqlValue <- Result.getValue result row column

    case fromSqlValue sqlValue of
      Right value ->
        pure (Right value)
      Left err ->
        let
          details =
            MarshallError.DecodingErrorDetails
              { MarshallError.decodingErrorValues = [(fieldNameToByteString sourceFieldName, sqlValue)]
              , MarshallError.decodingErrorMessage = err
              }
        in
          pure (Left $ MarshallError.DecodingError details)

{- | A 'RowIdentityExtractor' is used to retrieve identifying information for a
  row when a 'MarshallError.MarshallError' occurs reading it from the database.

  You should only need to worry about this type if you're using
  'marshallResultFromSqlUsingRowIdExtractor' and need to manually provide it.
  When possible, it's easier to annotate a 'SqlMarshaller' with the field names
  you would like rows to be identified by and then use 'marshallResultFromSql'
  instead.

@since 1.0.0.0
-}
newtype RowIdentityExtractor
  = RowIdentityExtractor (Row -> IO [(B8.ByteString, SqlValue.SqlValue)])

{- | Constructs a 'RowIdentityExtractor' that will extract values for the given
  fields from the result set to identify rows in decoding errors. Any of the
  named fields that are missing from the result set will not be included in the
  extracted row identity.

@since 1.0.0.0
-}
mkRowIdentityExtractor ::
  ExecutionResult result =>
  [FieldName] ->
  result ->
  RowIdentityExtractor
mkRowIdentityExtractor fields result =
  RowIdentityExtractor $ \row -> do
    let
      fieldNameSet =
        Set.fromList
          . fmap fieldNameToByteString
          $ fields

      getIdentityValue columnNumber = do
        mbColumnName <- Result.columnName result columnNumber

        case mbColumnName of
          Just name | Set.member name fieldNameSet -> do
            value <- Result.getValue result row columnNumber
            pure $ Just (name, value)
          _ ->
            pure Nothing

    mbMaxColumn <- Result.maxColumnNumber result

    case mbMaxColumn of
      Nothing ->
        pure []
      Just maxColumn ->
        fmap catMaybes $ traverse getIdentityValue [Column 0 .. maxColumn]

{- | Builds a 'SqlMarshaller' that maps a single field of a Haskell entity to
  a single column in the database. That value to store in the database will be
  retrieved from the entity using a provided accessor function. This function
  is intended to be used inside of a stanza of 'Applicative' syntax that will
  pass values read from the database to a constructor function to rebuild the
  entity containing the field, like so:

  @

  data Foo = Foo { bar :: Int32, baz :: Text }

  fooMarshaller :: SqlMarshaller Foo Foo
  fooMarshaller =
    Foo
      \<$\> marshallField bar (integerField "bar")
      \<*\> marshallField baz (unboundedTextField "baz")

  @

@since 1.0.0.0
-}
marshallField ::
  (writeEntity -> fieldValue) ->
  FieldDefinition nullability fieldValue ->
  SqlMarshaller writeEntity fieldValue
marshallField accessor fieldDef =
  MarshallNest accessor (MarshallField fieldDef)

{- | Builds a 'SqlMarshaller' that will include a SQL expression in select
  statements to calculate a value using the columns of the table being selected
  from. The columns being used in the calculation do not themselves need
  to be selected, though they must be present in the table so they can
  be referenced.

  @
  data AgeCheck
    { atLeast21 :: Bool
    }

  fooMarshaller :: SqlMarshaller Void AgeCheck
  fooMarshaller =
    AgeCheck
      \<*\> Orville.marshallSyntheticField atLeast21Field

  atLeast21Field :: SyntheticField Bool
  atLeast21Field =
    SyntheticField
      { syntheticFieldExpression = RawSql.unsafeSqlExpression "age >= 21"
      , syntheticFieldAlias = Orville.stringToFieldName "over21"
      , syntheticFieldValueFromSqlValue = SqlValue.toBool
      }
  @

@since 1.0.0.0
-}
marshallSyntheticField ::
  SyntheticField fieldValue ->
  SqlMarshaller writeEntity fieldValue
marshallSyntheticField =
  MarshallSyntheticField

{- | Nests a 'SqlMarshaller' inside another, using the given accessor to retrieve
  values to be marshalled. The resulting marshaller can then be used in the same
  way as 'marshallField' within the applicative syntax of a larger marshaller.

  For Example:

  @
  data Person =
    Person
      { personId :: PersonId
      , personName :: Name
      }

  data Name =
    Name
      { firstName :: Text
      , lastName :: Text
      }

  personMarshaller :: SqlMarshaller Person Person
  personMarshaller =
    Person
      \<$\> marshallField personId personIdField
      \<*\> marshallNested personName nameMarshaller

  nameMarshaller :: SqlMarshaller Name Name
  nameMarshaller =
    Name
      \<$\> marshallField firstName firstNameField
      \<*\> marshallField lastName lastNameField
  @

@since 1.0.0.0
-}
marshallNested ::
  (parentEntity -> nestedWriteEntity) ->
  SqlMarshaller nestedWriteEntity nestedReadEntity ->
  SqlMarshaller parentEntity nestedReadEntity
marshallNested =
  MarshallNest

{- | Lifts a 'SqlMarshaller' to have both read/write entities be 'Maybe',
  and applies a tag to avoid double mapping.

@since 1.0.0.0
-}
marshallMaybe :: SqlMarshaller a b -> SqlMarshaller (Maybe a) (Maybe b)
marshallMaybe =
  -- rewrite the mapper to handle null fields, then tag
  -- it as having been done so we don't double-map it
  -- in a future 'maybeMapper' call.
  MarshallMaybeTag . go
 where
  go :: SqlMarshaller a b -> SqlMarshaller (Maybe a) (Maybe b)
  go marshaller =
    case marshaller of
      MarshallPure a ->
        MarshallPure $ pure a
      MarshallApply func a ->
        MarshallApply (fmap (<*>) $ go func) (go a)
      MarshallNest f a ->
        MarshallNest (fmap f) (go a)
      (MarshallMaybeTag _) ->
        Just <$> MarshallNest join marshaller
      MarshallField field ->
        case fieldNullability field of
          NotNullField f -> MarshallField (nullableField f)
          NullableField f -> MarshallField (asymmetricNullableField f)
      MarshallSyntheticField synthField ->
        MarshallSyntheticField (nullableSyntheticField synthField)
      MarshallPartial m ->
        MarshallPartial (fmap sequence $ go m)
      MarshallReadOnly m ->
        MarshallReadOnly (go m)
      MarshallAlias a m ->
        MarshallAlias a (go m)

{- | Builds a 'SqlMarshaller' that will raise a decoding error when the value
  produced is a 'Left'.

@since 1.0.0.0
-}
marshallPartial :: SqlMarshaller a (Either String b) -> SqlMarshaller a b
marshallPartial = MarshallPartial

{- | Builds a 'SqlMarshaller' that will qualifiy the fields contained with the given alias.

@since 1.1.0.0
-}
marshallAlias ::
  AliasName ->
  SqlMarshaller a b ->
  SqlMarshaller a b
marshallAlias =
  MarshallAlias

{- | Adds a prefix, followed by an underscore, to the names of all of the fields
  and synthetic fields in a 'SqlMarshaller'.

@since 1.0.0.0
-}
prefixMarshaller ::
  String ->
  SqlMarshaller readEntity writeEntity ->
  SqlMarshaller readEntity writeEntity
prefixMarshaller prefix = go
 where
  go :: SqlMarshaller a b -> SqlMarshaller a b
  go marshaller = case marshaller of
    MarshallPure b -> MarshallPure b
    MarshallApply m1 m2 ->
      MarshallApply (go m1) $ go m2
    MarshallNest f m ->
      MarshallNest f $ go m
    MarshallField fieldDefinition ->
      MarshallField $ prefixField prefix fieldDefinition
    MarshallSyntheticField syntheticField ->
      MarshallSyntheticField $ prefixSyntheticField prefix syntheticField
    MarshallMaybeTag m -> MarshallMaybeTag $ go m
    MarshallPartial m -> MarshallPartial $ go m
    MarshallReadOnly m -> MarshallReadOnly $ go m
    MarshallAlias a m -> MarshallAlias a $ go m

{- | Marks a 'SqlMarshaller' as read-only so that it will not attempt to
  read any values from the @writeEntity@. You should use this if you have
  a group of fields which are populated by database rather than the application.

@since 1.0.0.0
-}
marshallReadOnly :: SqlMarshaller a b -> SqlMarshaller c b
marshallReadOnly = MarshallReadOnly

{- | A version of 'marshallField' that uses 'marshallReadOnly' to make a single
  read-only field. You will usually use this in conjunction with a
  'FieldDefinition' like @serialField@ where the value is populated by the
  database.

@since 1.0.0.0
-}
marshallReadOnlyField ::
  FieldDefinition nullability fieldValue ->
  SqlMarshaller writeEntity fieldValue
marshallReadOnlyField = MarshallReadOnly . MarshallField

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is equal to the given value encoded as a row.

@since 1.1.0.0
-}
marshallerEquals :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerEquals = SqlComparable.equals

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is not equal to the given value encoded as a row.

@since 1.1.0.0
-}
marshallerNotEquals :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerNotEquals = SqlComparable.notEquals

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is distinct from the given value encoded as a row.

@since 1.1.0.0
-}
marshallerIsDistinctFrom :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerIsDistinctFrom = SqlComparable.isDistinctFrom

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is not distinct from the given value encoded as a row.

@since 1.1.0.0
-}
marshallerIsNotDistinctFrom :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerIsNotDistinctFrom = SqlComparable.isNotDistinctFrom

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is less than the given value encoded as a row.

@since 1.1.0.0
-}
marshallerLessThan :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerLessThan = SqlComparable.lessThan

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is less than or equal to the given value encoded as a row.

@since 1.1.0.0
-}
marshallerLessThanOrEqualTo :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerLessThanOrEqualTo = SqlComparable.lessThanOrEqualTo

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is greater than the given value encoded as a row.

@since 1.1.0.0
-}
marshallerGreaterThan :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerGreaterThan = SqlComparable.greaterThan

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is greater than or equal to the given value encoded as a row.

@since 1.1.0.0
-}
marshallerGreaterThanOrEqualTo :: SqlMarshaller writeEntity x -> writeEntity -> Expr.BooleanExpr
marshallerGreaterThanOrEqualTo = SqlComparable.greaterThanOrEqualTo

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is in the given list of values encoded as a list of rows.

@since 1.1.0.0
-}
marshallerIn :: SqlMarshaller writeEntity x -> NE.NonEmpty writeEntity -> Expr.BooleanExpr
marshallerIn = SqlComparable.isIn

{- | Checks that the row value containing the write fields of the
  'SqlMarshaller' is not in the given list of values encoded as a list of rows.

@since 1.1.0.0
-}
marshallerNotIn :: SqlMarshaller writeEntity x -> NE.NonEmpty writeEntity -> Expr.BooleanExpr
marshallerNotIn = SqlComparable.isNotIn
