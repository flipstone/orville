{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Orville.TypeId.FieldDefinition
  ( kindIDDefaultValue
  , kindIDFieldDefinitionWithoutDefault
  , kindIDFieldDefinitionWithDefault
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.KindID as KindID
import qualified Data.KindID.Class as KindIDClass
import GHC.Exts (proxy#)
import GHC.TypeLits (KnownSymbol, symbolVal')
import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Default value for a 'KindID' generated with the provided prefix

'KindID' is a haskell representation of https://github.com/jetify-com/typeid

Note that 'typeid_generate_text' (and its dependents) need to be loaded as postgres functions using Orville 'SchemaFunction'

@since 0.1.0.0
-}
kindIDDefaultValue ::
  forall prefix.
  KnownSymbol prefix =>
  Orville.DefaultValue (KindID.KindID prefix)
kindIDDefaultValue =
  Orville.rawSqlDefault
    . RawSql.unsafeFromRawSql
    -- See documentation on 'O.rawSqlDefault' if changing - this must exactly match @pg_get_expr@
    $ RawSql.fromString "typeid_generate_text("
      <> RawSql.stringLiteral (BS8.pack $ symbolVal' (proxy# @prefix))
      <> RawSql.fromString "::text)"

{- | Field definition for a 'KindID' with the provided prefix

'KindID' is a haskell representation of https://github.com/jetify-com/typeid

@since 0.1.0.0
-}
kindIDFieldDefinitionWithoutDefault ::
  forall prefix.
  KindIDClass.ValidPrefix prefix =>
  String ->
  Orville.FieldDefinition Orville.NotNull (KindID.KindID prefix)
kindIDFieldDefinitionWithoutDefault fieldName =
  let
    fromText = either (Left . show) (Right . id) . KindID.parseText @prefix
    convertToKindID = Orville.tryConvertSqlType KindID.toText fromText
  in
    Orville.convertField convertToKindID $
      Orville.boundedTextField fieldName (63 + 1 + 26) -- prefix + underscore + uuid encoded suffix

{- | Field definition for a 'KindID' that provides a default value generated in postgresql with the provided prefix

'KindID' is a haskell representation of https://github.com/jetify-com/typeid

Note that 'typeid_generate_text' (and its dependents) need to be loaded as postgres functions using Orville 'SchemaFunction'

@since 0.1.0.0
-}
kindIDFieldDefinitionWithDefault ::
  forall prefix.
  KindIDClass.ValidPrefix prefix =>
  String ->
  Orville.FieldDefinition Orville.NotNull (KindID.KindID prefix)
kindIDFieldDefinitionWithDefault =
  Orville.setDefaultValue kindIDDefaultValue . kindIDFieldDefinitionWithoutDefault
