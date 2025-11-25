{-# LANGUAGE ScopedTypeVariables #-}

module Orville.PostgreSQL.Internal.TypeMap
  ( TypeMap
  , empty
  , insert
  , lookup
  ) where

import Prelude (Maybe, fmap)

import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (Proxy))
import GHC.Base (Any)
import Type.Reflection (SomeTypeRep, Typeable, someTypeRep)
import qualified Unsafe.Coerce as UnsafeCoerce

{- |
  A type map can be used to store dynamic values keyed by their
  type. Orville uses this internally to store custome user data to allow
  extensibility without needing to track all user data in every function's
  type signature that deals with the type the user data is attached to
  (e.g. FieldDefinition)
-}
newtype TypeMap = TypeMap (Map.Map SomeTypeRep Any)

{- |
  An empty 'TypeMap' with no values.
-}
empty :: TypeMap
empty = TypeMap Map.empty

{- |
  Inserts a value into the 'TypeMap'. Any existing value of the same type
  will be overwritten.
-}
insert :: forall a. Typeable a => a -> TypeMap -> TypeMap
insert a (TypeMap m) =
  let
    typeRep =
      someTypeRep (Proxy :: Proxy a)
  in
    TypeMap (Map.insert typeRep (UnsafeCoerce.unsafeCoerce a) m)

{- |
  Looks up a value from the 'TypeMap'. The value retrieved is based on the
  type of the return value based. Type applications can be used, if desired,
  to determine the return type directly.
-}
lookup :: forall a. Typeable a => TypeMap -> Maybe a
lookup (TypeMap m) =
  let
    typeRep =
      someTypeRep (Proxy :: Proxy a)
  in
    fmap UnsafeCoerce.unsafeCoerce (Map.lookup typeRep m)
