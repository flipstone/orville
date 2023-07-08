module Orville.PostgreSQL.Plan.Many
  ( Many
  , NotAKey (NotAKey)
  , fromKeys
  , lookup
  , keys
  , elems
  , map
  , toMap
  , apply
  , compose
  )
where

import Prelude (Either (Left, Right), Functor (fmap), Maybe (Just, Nothing), Ord, ($), (.), (<*>))

import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

{- |
  'NotAKey' is returned from various 'Many' related functions when presented
  with an input parameter that was not one of the original inputs that the
  'Many' was constructed with.
-}
data NotAKey
  = NotAKey

{- |
  A 'Many k a' represents a group of values keyed by list of parameters and
  is used to return the results of executing an Orville Plan with a list of
  input parameters. If you need to find the result of the query associated
  with a particular input parameter, you can use 'lookup' to find it. If you
  don't care about the association with particular inputs, you can simply
  use 'elems' to get a list of all the results.
-}
data Many k a
  = Many [k] (k -> Either NotAKey a)

instance Functor (Many k) where
  fmap = map

{- |
  'fromKeys' constructs a 'Many' value from a list of keys and a function that
  maps them to their values. The order and duplication of keys in the list will
  be preserved by the 'Many' type in the relevant functions. The mapping
  function provided should be a total function -- i.e. it should not produce a
  runtime error. If it is not possible to map every @k@ (even those not in the
  input list provided to 'fromKeys'), the values should be wrapped in an
  appropriate type such as 'Maybe' so that an empty or default value can be
  returned.
-}
fromKeys :: [k] -> (k -> Either NotAKey a) -> Many k a
fromKeys =
  Many

{- |
   'map' calls a function on all the values found in a 'Many' collection.
-}
map :: (a -> b) -> Many k a -> Many k b
map f (Many ks keyToValue) =
  Many ks (fmap f . keyToValue)

{- |
   'apply' allows you to apply many functions to many values. The function
   associated with each parameter is applied to the value associated with the
   same paremeter.

   (If you're looking for 'pure' or an 'Applicative' instance for 'Many', this
   is as good as it gets. 'Many' cannot be an 'Applicative' because there is no
   correct implementation of 'pure' that we can reasonably provide).
-}
apply ::
  (Many param (a -> b)) ->
  (Many param a) ->
  (Many param b)
apply manyFs manyAs =
  fromKeys (keys manyFs) applyF
 where
  applyF param =
    lookup param manyFs <*> lookup param manyAs

{- |
  'compose' uses the values of a 'Many' value as keys to a second 'Many' to
  create a 'Many' mapping from the original keys to the final values.
-}
compose :: Many b c -> Many a b -> Many a c
compose manyBC manyAB =
  fromKeys (keys manyAB) aToC
 where
  aToC a = do
    b <- lookup a manyAB
    lookup b manyBC

{- |
  'keys' fetches the list of keys from a 'Many'. Note that is a list and not
  a set. 'Many' preserves the order and duplication of any key values that were
  in the key list at the time of construction.
-}
keys :: Many k a -> [k]
keys (Many ks _) =
  ks

{- |
  'elems' returns all the values that correspond the keys of the 'Many'. The
  values will be returned in the same order that the keys were present at the
  time of creation, though if you truly care about this it's probably better to
  use 'lookup' to make that correspondence explicit.
-}
elems :: Many k a -> [a]
elems (Many ks keyToValue) =
  Either.rights $ fmap keyToValue ks

{- |
  'toMap' converts the 'Many' into a 'Map' value. If all you wanted to do was
  find the value for a specific key, you should probably use 'lookup' instead.
-}
toMap :: Ord k => Many k a -> Map.Map k a
toMap (Many ks keyToValue) =
  Map.fromList (Maybe.mapMaybe mkPair ks)
 where
  mkPair k =
    case keyToValue k of
      Left NotAKey ->
        Nothing
      Right value ->
        Just (k, value)

{- |
  'lookup' returns the value for the given parameter. If the given @k@ is
  not one of the original input values that the 'Many' was constructed with,
  the mapping function given at the contructor will determine what value to
  return. Often this will be whatever a reasonable empty or default value for
  the type @a@ is.
-}
lookup :: k -> Many k a -> Either NotAKey a
lookup k (Many _ keyToValue) =
  keyToValue k
