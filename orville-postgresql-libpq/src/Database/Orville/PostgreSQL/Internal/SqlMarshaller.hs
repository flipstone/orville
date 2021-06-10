{-|
Module    : Database.Orville.PostgreSQL.Internal.SqlMarshaller
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Database.Orville.PostgreSQL.Internal.SqlMarshaller
  ( SqlMarshaller
  , MarshallError(..)
  , marshallEntityToSql
  , marshallResultFromSql
  , marshallRowFromSql
  , marshallField
  , mkRowSource
  , RowSource
  , mapRowSource
  , applyRowSource
  , constRowSource
  , failRowSource
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)

import           Database.Orville.PostgreSQL.Internal.ExecutionResult (ExecutionResult, Column(Column), Row(Row))
import qualified Database.Orville.PostgreSQL.Internal.ExecutionResult as Result
import           Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, FieldName, fieldName, fieldNameToByteString, fieldValueFromSqlValue, fieldValueToSqlValue)
import           Database.Orville.PostgreSQL.Internal.SqlValue (SqlValue)

{-|
  'SqlMarshaller' is how we group the lowest level translation of single fields
  into a higher level marshalling of full sql records into Haskell records.
  This is a flexible abstraction that allows us to ultimately model SQL tables
  and work with them as potentially nested Haskell records.  We can then
  "marshall" the data as we want to model it in sql and Haskell.
-}
data SqlMarshaller a b where
  -- | Our representation of `pure` in the `Applicative` sense
  MarshallPure  :: b -> SqlMarshaller a b
  -- | Representation of application like `<*>` from `Applicative`
  MarshallApply :: SqlMarshaller a (b -> c) -> SqlMarshaller a b -> SqlMarshaller a c

  -- | Nest an arbitrary function, this is used when modeling a SQL table as nested Haskell records
  MarshallNest :: (a -> b) -> SqlMarshaller b c -> SqlMarshaller a c

  -- | Mashall a SQL column using the given 'FieldDefinition'
  MarshallField :: FieldDefinition nullability a -> SqlMarshaller a a

instance Functor (SqlMarshaller a) where
  fmap f marsh = MarshallApply (MarshallPure f) marsh

instance Applicative (SqlMarshaller a) where
  pure = MarshallPure
  (<*>) = MarshallApply

{-|
  Encodes an Haskell entity to the `FieldName`/`SqlValue` pairs that are
  described by a `SqlMarshaller`. Each pair is passed to the result-building
  function to allow to caller to construct the `result` type as desired.
  The `result` is built in a right-associative manner, much like a `foldr`
  operation.
-}
marshallEntityToSql :: SqlMarshaller writeEntity readEntity
                    -> result
                    -> (FieldName -> SqlValue -> result -> result)
                    -> writeEntity
                    -> result
marshallEntityToSql marshaller currentResult addToResult entity =
  case marshaller of
    MarshallPure _ ->
      currentResult

    MarshallApply submarshallerA submarshallerB ->
      let
        subresultB =
          marshallEntityToSql submarshallerB currentResult addToResult entity
      in
        marshallEntityToSql submarshallerA subresultB addToResult entity

    MarshallNest nestingFunction submarshaller ->
      marshallEntityToSql submarshaller currentResult addToResult (nestingFunction entity)

    MarshallField fieldDefinition ->
      addToResult
        (fieldName fieldDefinition)
        (fieldValueToSqlValue fieldDefinition entity)
        currentResult

{-|
  A 'MarshallError' may be returned from 'marshallFromSql' if the result set
  being decoded from the database doesn't meet the expectations of the
  'SqlMarshaller' that is decoding it.
-}
data MarshallError
  = -- | Indicates that a particular value in a column could not be decoded
    FailedToDecodeValue
    -- | Indicates that an expected column was not found in the result set
  | FieldNotFoundInResultSet
  deriving Eq

-- NOTE: We want to be sure that the 'Show' instance of Marshall error does
-- not expose any values read from the database, so we implement it explicitly
-- here rather than deriving it.
instance Show MarshallError where
  show err =
    case err of
      FailedToDecodeValue      -> "FailedToDecodeValue"
      FieldNotFoundInResultSet -> "FieldNotFoundInResultSet"

{-|
  Decodes all the rows found in a execution result at once. The first row that
  fails to decode will return the `MarshallError` that results, otherwise all
  decoded rows will be returned.

  Note that this function loads are decoded rows into memory at once, so it
  should only be used with result sets that you know will fit into memory.
-}
marshallResultFromSql :: ExecutionResult result
                      => SqlMarshaller writeEntity readEntity
                      -> result
                      -> IO (Either MarshallError [readEntity])
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
                    pure (Right (b:bs))


{-|
  A `RowSource` can fetch and decode rows from a database result set. Using
  a `RowSource` gives random access to the rows in the result set, only
  attempting to decode them when they are requested by the use via `decodeRow`.

  Note that even though the rows are not decoded into Haskell until `decodeRow`
  is called, all the rows returned from the query are held in memory on the
  client waiting to be decoded until the `RowSource` is garbage collected.
  As such, you can't use `RowSource` (alone) to achieve any form of streaming
  or pagination of rows between the database server and the client.
-}
newtype RowSource readEntity =
  RowSource (Row -> IO (Either MarshallError readEntity))

instance Functor RowSource where
  fmap = mapRowSource

instance Applicative RowSource where
  pure = constRowSource
  (<*>) = applyRowSource

{-|
  Attempts to decode a result set row that has already been fetched from the
  database server into a Haskell value. If the decoding fails, a `MarshallError`
  will be returned.
-}
decodeRow :: RowSource readEntity -> Row -> IO (Either MarshallError readEntity)
decodeRow (RowSource source) =
  source

{-|
  Adds a function to the decoding proocess to transform the value returned
  by a `RowSource`.
-}
mapRowSource :: (a -> b) -> RowSource a -> RowSource b
mapRowSource f (RowSource decodeA) =
  RowSource $ \row -> fmap (fmap f) (decodeA row)

{-|
  Creates a `RowSource` that always returns the value given, rather than
  attempting to access the result set and decoding anything.
-}
constRowSource :: readEntity -> RowSource readEntity
constRowSource =
  RowSource . const . pure . Right

{-|
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

{-|
  Creates a `RowSource` that will always fail to decode by returning the
  provided error. This can be used in cases where a `RowSource` must
  be provided but it is already known at run time that decoding is impossible.
  For instance, this is used internally when a `FieldDefinition` references
  a column that does not exist in the result set.
-}
failRowSource :: MarshallError -> RowSource a
failRowSource =
  RowSource . const . pure . Left

{-|
  Uses the `SqlMarshaller` given to build a `RowSource` that will decode
  from the given result set. The returned `RowSource` can then be used to
  decode rows as desired by the user. Note that the entire result set is
  held in memory for potential decoding until the `RowSource` is garbage
  collected.
-}
mkRowSource :: ExecutionResult result
            => SqlMarshaller writeEntity readEntity
            -> result
            -> IO (RowSource readEntity)
mkRowSource marshaller result = do
  columnMap <- prepareColumnMap result

  let
    mkSource :: SqlMarshaller a b -> RowSource b
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
          let
            fieldNameBytes =
              fieldNameToByteString (fieldName fieldDef)
          in
            case Map.lookup fieldNameBytes columnMap of
              Just columnNumber ->
                mkColumnRowSource fieldDef result columnNumber

              Nothing ->
                failRowSource FieldNotFoundInResultSet

  pure . mkSource $ marshaller

{-|
  Decodes a result set row from the database using the given 'SqlMarshaller'.
  This will lookup the values in the result set based on the field names in all
  the 'FieldDefinition's used in the 'SqlMarshaller' and attempt to convert the
  'SqlValue's into their more specific Haskell types. Any failures in this
  process will cause the entire row to fail to decode and return a
  'MarshallError' in the result.
-}
marshallRowFromSql :: ExecutionResult result
                   => SqlMarshaller writeEntity readEntity -- ^ 'SqlMarshaller' to use for decoding
                   -> Row -- ^ A row number to decode
                   -> result -- ^ result set to decode from
                   -> IO (Either MarshallError readEntity)
marshallRowFromSql marshaller rowNumber result = do
  rowSource <- mkRowSource marshaller result
  decodeRow rowSource rowNumber

{-|
  An internal helper function that finds all the column names in a result set
  and associates them with the respective column numbers for easier lookup.
-}
prepareColumnMap :: ExecutionResult result
                 => result
                 -> IO (Map.Map B8.ByteString Column)
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

{-|
  A internal helper function for to build a `RowSource` that retrieves and
  decodes a single column value form the result set.
-}
mkColumnRowSource :: ExecutionResult result
                  => FieldDefinition nullability a
                  -> result
                  -> Column
                  -> RowSource a
mkColumnRowSource fieldDef result column =
  RowSource $ \row -> do
    sqlValue <- Result.getValue result row column

    case fieldValueFromSqlValue fieldDef sqlValue of
      Just value ->
        pure (Right value)

      Nothing ->
        pure (Left FailedToDecodeValue)


{-|
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
marshallField :: (writeEntity -> fieldValue)
             -> FieldDefinition nullability fieldValue
             -> SqlMarshaller writeEntity fieldValue
marshallField accessor fieldDef =
  MarshallNest accessor (MarshallField fieldDef)
