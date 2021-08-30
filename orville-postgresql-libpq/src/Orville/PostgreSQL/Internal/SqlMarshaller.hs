{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module    : Orville.PostgreSQL.Internal.SqlMarshaller
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.SqlMarshaller
  ( SqlMarshaller,
    MarshallError (..),
    marshallResultFromSql,
    marshallRowFromSql,
    marshallField,
    foldMarshallerFields,
    FieldFold,
    mkRowSource,
    RowSource,
    mapRowSource,
    applyRowSource,
    constRowSource,
    failRowSource,
    maybeMapper,
    partialMap,
    marshallReadOnly,
    marshallReadOnlyField,
  )
where

import Control.Exception (Exception)
import Control.Monad (join)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Internal.ExecutionResult (Column (Column), ExecutionResult, Row (Row))
import qualified Orville.PostgreSQL.Internal.ExecutionResult as Result
import Orville.PostgreSQL.Internal.FieldDefinition
  ( FieldDefinition,
    FieldNullability (NotNullField, NullableField),
    asymmetricNullableField,
    fieldName,
    fieldNameToByteString,
    fieldNullability,
    fieldValueFromSqlValue,
    nullableField,
  )

{- |
  'SqlMarshaller' is how we group the lowest level translation of single fields
  into a higher level marshalling of full sql records into Haskell records.
  This is a flexible abstraction that allows us to ultimately model SQL tables
  and work with them as potentially nested Haskell records.  We can then
  "marshall" the data as we want to model it in sql and Haskell.
-}
data SqlMarshaller a b where
  -- | Our representation of `pure` in the `Applicative` sense
  MarshallPure :: b -> SqlMarshaller a b
  -- | Representation of application like `<*>` from `Applicative`
  MarshallApply :: SqlMarshaller a (b -> c) -> SqlMarshaller a b -> SqlMarshaller a c
  -- | Nest an arbitrary function, this is used when modeling a SQL table as nested Haskell records
  MarshallNest :: (a -> b) -> SqlMarshaller b c -> SqlMarshaller a c
  -- | Marshall a SQL column using the given 'FieldDefinition'
  MarshallField :: FieldDefinition nullability a -> SqlMarshaller a a
  -- | Tag a maybe-mapped marshaller so we don't map it twice
  MarshallMaybeTag :: SqlMarshaller (Maybe a) (Maybe b) -> SqlMarshaller (Maybe a) (Maybe b)
  -- | Marshall a column with a possibility of error
  MarshallPartial :: SqlMarshaller a (Either MarshallError a) -> SqlMarshaller a a
  -- | Marshall a column that is read only, like auto-incrementing ids
  MarshallReadOnly :: SqlMarshaller a b -> SqlMarshaller c b

instance Functor (SqlMarshaller a) where
  fmap f marsh = MarshallApply (MarshallPure f) marsh

instance Applicative (SqlMarshaller a) where
  pure = MarshallPure
  (<*>) = MarshallApply

{- |
  A synonym that captures the type of folding functions that are used
  with 'foldMarshallerFields'. Note that this synonym is defined using
  'RankNTypes' because the folding function must consume all fields,
  regardless of they type of value they contain or their nullability.
  Although you do not need to enable 'RankNTypes' simply to use this
  synonym as the type signature of your own folding functions, you may
  need to enable it to write an explicit 'forall' if your folding function
  is built from parameters that include a field's nullability or value type
  in their signature.
-}
type FieldFold writeEntity result =
  forall a nullability.
  FieldDefinition nullability a ->
  Maybe (writeEntity -> a) ->
  result ->
  result

{- |
  'foldMarshallerFields' allows you to consume the 'FieldDefinition's that
  are contained within the 'SqlMarshaller' to process them however is
  required. This can be used to collect the names of all the fields, encode
  them to 'SqlValue', etc.
-}
foldMarshallerFields ::
  SqlMarshaller writeEntity readEntity ->
  result ->
  FieldFold writeEntity result ->
  result
foldMarshallerFields marshaller =
  foldMarshallerFieldsPart marshaller (Just id)

{- |
  The internal helper function that actually implements 'foldMarshallerFields'.
  It takes with it a function that extracts the current nesting entity from
  the overall 'writeEntity' that the 'SqlMarshaller' is build on. 'MarshallNest'
  adds more nesting by composing its accessor with the one given here.
-}
foldMarshallerFieldsPart ::
  SqlMarshaller entityPart readEntity ->
  Maybe (writeEntity -> entityPart) ->
  result ->
  FieldFold writeEntity result ->
  result
foldMarshallerFieldsPart marshaller getPart currentResult addToResult =
  case marshaller of
    MarshallPure _ ->
      currentResult
    MarshallApply submarshallerA submarshallerB ->
      let subresultB =
            foldMarshallerFieldsPart submarshallerB getPart currentResult addToResult
       in foldMarshallerFieldsPart submarshallerA getPart subresultB addToResult
    MarshallNest nestingFunction submarshaller ->
      foldMarshallerFieldsPart submarshaller (fmap (nestingFunction .) getPart) currentResult addToResult
    MarshallField fieldDefinition ->
      addToResult fieldDefinition getPart currentResult
    MarshallMaybeTag m ->
      foldMarshallerFieldsPart m getPart currentResult addToResult
    MarshallPartial m ->
      foldMarshallerFieldsPart m getPart currentResult addToResult
    MarshallReadOnly m ->
      foldMarshallerFieldsPart m Nothing currentResult addToResult

{- |
  A 'MarshallError' may be returned from 'marshallFromSql' if the result set
  being decoded from the database doesn't meet the expectations of the
  'SqlMarshaller' that is decoding it.
-}
data MarshallError
  = -- | Indicates that a particular value in a column could not be decoded
    FailedToDecodeValue
  | -- | Indicates that an expected column was not found in the result set
    FieldNotFoundInResultSet
  deriving (Eq)

-- NOTE: We want to be sure that the 'Show' instance of Marshall error does
-- not expose any values read from the database, so we implement it explicitly
-- here rather than deriving it.
instance Show MarshallError where
  show err =
    case err of
      FailedToDecodeValue -> "FailedToDecodeValue"
      FieldNotFoundInResultSet -> "FieldNotFoundInResultSet"

instance Exception MarshallError

{- |
  Decodes all the rows found in a execution result at once. The first row that
  fails to decode will return the `MarshallError` that results, otherwise all
  decoded rows will be returned.

  Note that this function loads are decoded rows into memory at once, so it
  should only be used with result sets that you know will fit into memory.
-}
marshallResultFromSql ::
  ExecutionResult result =>
  SqlMarshaller writeEntity readEntity ->
  result ->
  IO (Either MarshallError [readEntity])
marshallResultFromSql marshaller result = do
  mbMaxRow <- Result.maxRowNumber result

  case mbMaxRow of
    Nothing ->
      pure (Right [])
    Just maxRow -> do
      rowSource <- mkRowSource marshaller result
      traverseSequence (decodeRow rowSource) [Row 0 .. maxRow]

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

{- |
  A `RowSource` can fetch and decode rows from a database result set. Using
  a `RowSource` gives random access to the rows in the result set, only
  attempting to decode them when they are requested by the use via `decodeRow`.

  Note that even though the rows are not decoded into Haskell until `decodeRow`
  is called, all the rows returned from the query are held in memory on the
  client waiting to be decoded until the `RowSource` is garbage collected.
  As such, you can't use `RowSource` (alone) to achieve any form of streaming
  or pagination of rows between the database server and the client.
-}
newtype RowSource readEntity
  = RowSource (Row -> IO (Either MarshallError readEntity))

instance Functor RowSource where
  fmap = mapRowSource

instance Applicative RowSource where
  pure = constRowSource
  (<*>) = applyRowSource

{- |
  Attempts to decode a result set row that has already been fetched from the
  database server into a Haskell value. If the decoding fails, a `MarshallError`
  will be returned.
-}
decodeRow :: RowSource readEntity -> Row -> IO (Either MarshallError readEntity)
decodeRow (RowSource source) =
  source

{- |
  Adds a function to the decoding proocess to transform the value returned
  by a `RowSource`.
-}
mapRowSource :: (a -> b) -> RowSource a -> RowSource b
mapRowSource f (RowSource decodeA) =
  RowSource $ \row -> fmap (fmap f) (decodeA row)

{- |
  Creates a `RowSource` that always returns the value given, rather than
  attempting to access the result set and decoding anything.
-}
constRowSource :: readEntity -> RowSource readEntity
constRowSource =
  RowSource . const . pure . Right

{- |
  Applies a function that will be decoded from the result set to another
  value decode from the result set.
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

{- |
  Creates a `RowSource` that will always fail to decode by returning the
  provided error. This can be used in cases where a `RowSource` must
  be provided but it is already known at run time that decoding is impossible.
  For instance, this is used internally when a `FieldDefinition` references
  a column that does not exist in the result set.
-}
failRowSource :: MarshallError -> RowSource a
failRowSource =
  RowSource . const . pure . Left

{- |
  Uses the `SqlMarshaller` given to build a `RowSource` that will decode
  from the given result set. The returned `RowSource` can then be used to
  decode rows as desired by the user. Note that the entire result set is
  held in memory for potential decoding until the `RowSource` is garbage
  collected.
-}
mkRowSource ::
  ExecutionResult result =>
  SqlMarshaller writeEntity readEntity ->
  result ->
  IO (RowSource readEntity)
mkRowSource marshaller result = do
  columnMap <- prepareColumnMap result

  let mkSource :: SqlMarshaller a b -> RowSource b
      mkSource marshallerPart =
        -- Note, this case statement is evaluated before the row argument is
        -- ever passed to a `RowSource` to ensure that a single `RowSource`
        -- operation is build and re-used when decoding many rows.
        case marshallerPart of
          MarshallPure readEntity ->
            constRowSource readEntity
          MarshallApply marshallAToB marshallA ->
            mkSource marshallAToB <*> mkSource marshallA
          MarshallNest _ someMarshaller ->
            mkSource someMarshaller
          MarshallField fieldDef ->
            let fieldNameBytes =
                  fieldNameToByteString (fieldName fieldDef)
             in case Map.lookup fieldNameBytes columnMap of
                  Just columnNumber ->
                    mkColumnRowSource fieldDef result columnNumber
                  Nothing ->
                    failRowSource FieldNotFoundInResultSet
          MarshallMaybeTag m ->
            mkSource m
          MarshallPartial m ->
            (\(RowSource f) -> RowSource (fmap join . f)) $ mkSource m
          MarshallReadOnly m ->
            mkSource m

  pure . mkSource $ marshaller

{- |
  Decodes a result set row from the database using the given 'SqlMarshaller'.
  This will lookup the values in the result set based on the field names in all
  the 'FieldDefinition's used in the 'SqlMarshaller' and attempt to convert the
  'SqlValue's into their more specific Haskell types. Any failures in this
  process will cause the entire row to fail to decode and return a
  'MarshallError' in the result.
-}
marshallRowFromSql ::
  ExecutionResult result =>
  -- | 'SqlMarshaller' to use for decoding
  SqlMarshaller writeEntity readEntity ->
  -- | A row number to decode
  Row ->
  -- | result set to decode from
  result ->
  IO (Either MarshallError readEntity)
marshallRowFromSql marshaller rowNumber result = do
  rowSource <- mkRowSource marshaller result
  decodeRow rowSource rowNumber

{- |
  An internal helper function that finds all the column names in a result set
  and associates them with the respective column numbers for easier lookup.
-}
prepareColumnMap ::
  ExecutionResult result =>
  result ->
  IO (Map.Map B8.ByteString Column)
prepareColumnMap result = do
  mbMaxColumn <- Result.maxColumnNumber result

  let mkNameEntry columnNumber = do
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

{- |
  A internal helper function for to build a `RowSource` that retrieves and
  decodes a single column value form the result set.
-}
mkColumnRowSource ::
  ExecutionResult result =>
  FieldDefinition nullability a ->
  result ->
  Column ->
  RowSource a
mkColumnRowSource fieldDef result column =
  RowSource $ \row -> do
    sqlValue <- Result.getValue result row column

    case fieldValueFromSqlValue fieldDef sqlValue of
      Just value ->
        pure (Right value)
      Nothing ->
        pure (Left FailedToDecodeValue)

{- |
  Builds a 'SqlMarshaller' that maps a single field of a Haskell entity to
  a single column in the database. That value to store in the database will
  be retried from the entity using provided accessor function. This function
  is intended to be used inside of a stanza of 'Applicative' syntax that will
  pass values read from the database a constructor function to rebuild the
  entity containing the field, like so:

  @

  data Foo = Foo { bar :: Int32, baz :: Text }

  fooMarshaller :: SqlMarshaller Foo Foo
  fooMarshaller =
    Foo
      <$> marshallField bar (integerField "bar")
      <*> marshallField baz (unboundedTextField "baz")

  @
-}
marshallField ::
  (writeEntity -> fieldValue) ->
  FieldDefinition nullability fieldValue ->
  SqlMarshaller writeEntity fieldValue
marshallField accessor fieldDef =
  MarshallNest accessor (MarshallField fieldDef)

{- |
  Lifts a 'SqlMarshaller' to have both read/write entities be 'Maybe',
  and applies a tag to avoid double mapping.
-}
maybeMapper :: SqlMarshaller a b -> SqlMarshaller (Maybe a) (Maybe b)
maybeMapper =
  -- rewrite the mapper to handle null fields, then tag
  -- it as having been done so we don't double-map it
  -- in a future `maybeMapper` call.
  MarshallMaybeTag . go
  where
    go :: SqlMarshaller a b -> SqlMarshaller (Maybe a) (Maybe b)
    go (MarshallPure a) = MarshallPure $ pure a
    go (MarshallApply func a) = MarshallApply (fmap (<*>) $ go func) (go a)
    go (MarshallNest f a) = MarshallNest (fmap f) (go a)
    go a@(MarshallMaybeTag _) = Just <$> MarshallNest join a
    go (MarshallField field) =
      case fieldNullability field of
        NotNullField f -> MarshallField (nullableField f)
        NullableField f -> MarshallField (asymmetricNullableField f)
    go (MarshallPartial m) = MarshallPartial (fmap sequence $ go m)
    go (MarshallReadOnly m) = MarshallReadOnly (go m)

{- |
  Builds a 'SqlMarshaller' that will raise a decoding error when the value
  produced is a 'MarshallError'.
-}
partialMap :: SqlMarshaller a (Either MarshallError a) -> SqlMarshaller a a
partialMap = MarshallPartial

marshallReadOnly :: SqlMarshaller a b -> SqlMarshaller c b
marshallReadOnly = MarshallReadOnly

marshallReadOnlyField ::
  FieldDefinition nullability fieldValue ->
  SqlMarshaller writeEntity fieldValue
marshallReadOnlyField = MarshallReadOnly . MarshallField
