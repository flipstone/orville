{-|
Module    : Database.Orville.PostgreSQL.Internal.SqlMarshaller
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module Database.Orville.PostgreSQL.Internal.SqlMarshaller
  ( SqlMarshaller
  , marshallFromSql
  ) where

import qualified Data.ByteString as BS

-- | 'SqlMarshaller' is how we group the lowest level translation of single fields
-- into a higher level marshalling of full sql records into Haskell records.
-- This is a flexible abstraction that allows us to ultimately model SQL tables and work with them
-- as potentially nested Haskell records.
-- We can then "marshall" the data as we want to model it in sql and Haskell.
data SqlMarshaller a b where
-- Once we have a `FieldDefinition` we'd like to be able to marshall with it. For now this is here to sketch future development
--  MarshallField :: FieldDefinition a -> SqlMarshaller a a

  -- | Our representation of `pure` in the `Applicative` sense
  MarshallPure  :: b -> SqlMarshaller a b
  -- | Representation of application like `<*>` from `Applicative`
  MarshallApply :: SqlMarshaller a (b -> c) -> SqlMarshaller a b -> SqlMarshaller a c

  -- | Nest an arbitrary function, this is used when modeling a SQL table as nested Haskell records
  MarshallNest :: (a -> b) -> SqlMarshaller b c -> SqlMarshaller a c

instance Functor (SqlMarshaller a) where
  fmap f marsh = MarshallApply (MarshallPure f) marsh

instance Applicative (SqlMarshaller a) where
  pure = MarshallPure
  (<*>) = MarshallApply

marshallFromSql :: SqlMarshaller writeEntity readEntity -> [(String, BS.ByteString)] -> Either String readEntity
marshallFromSql marshaller sqlValues =
  case marshaller of
    MarshallPure readEntity ->
      pure readEntity
    MarshallApply marshallF marshallEntity ->
      (marshallFromSql marshallF sqlValues) <*>
      (marshallFromSql marshallEntity sqlValues)
    MarshallNest _ someMarshaller ->
      marshallFromSql someMarshaller sqlValues
