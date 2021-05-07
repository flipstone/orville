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
  , marshallFromSql
  , mashallField
  ) where

import           Database.Orville.PostgreSQL.Internal.FieldDefinition (FieldDefinition, fieldName, fieldValueFromSqlValue)
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
  Decodes a result set row from the database using the given 'SqlMarshaller'.
  This will lookup the values in the result set based on the field names in all
  the 'FieldDefinition's used in the 'SqlMarshaller' and attempt to convert the
  'SqlValue's into their more specific Haskell types. Any failures in this
  process will cause the entire row to fail to decode and return a
  'MarshallError' in the result.
-}
marshallFromSql :: SqlMarshaller writeEntity readEntity -- ^ 'SqlMarshaller' to use for decoding
                -> [(String, SqlValue)] -- ^ A row of field name, value pairs to decode
                -> Either MarshallError readEntity
marshallFromSql marshaller sqlValues =
  case marshaller of
    MarshallPure readEntity ->
      pure readEntity

    MarshallApply marshallF marshallEntity ->
      (marshallFromSql marshallF sqlValues) <*>
      (marshallFromSql marshallEntity sqlValues)

    MarshallNest _ someMarshaller ->
      marshallFromSql someMarshaller sqlValues

    MarshallField fieldDef ->
      marshallFieldFromSql fieldDef sqlValues

{-|
  A internal helper function for decoding a single field from a result set row
-}
marshallFieldFromSql :: FieldDefinition nullability a -> [(String, SqlValue)] -> Either MarshallError a
marshallFieldFromSql fieldDef sqlValues =
  case lookup (fieldName fieldDef) sqlValues of
    Just sqlValue ->
      case fieldValueFromSqlValue fieldDef sqlValue of
        Just value ->
          Right value

        Nothing ->
          Left FailedToDecodeValue

    Nothing ->
      Left FieldNotFoundInResultSet

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
mashallField :: (writeEntity -> fieldValue)
             -> FieldDefinition nullability fieldValue
             -> SqlMarshaller writeEntity fieldValue
mashallField accessor fieldDef =
  MarshallNest accessor (MarshallField fieldDef)
