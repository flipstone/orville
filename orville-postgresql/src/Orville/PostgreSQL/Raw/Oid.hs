{- |
Copyright : Flipstone Technology Partners 2026
License   : MIT
Stability : Stable

@since 1.2.0.0
-}
module Orville.PostgreSQL.Raw.Oid
  ( Oid (Oid)
  )
where

import Data.Word (Word32)

{- | A PostgreSQL object identifier, as found in the @oid@ columns of the
  @pg_catalog@ tables and used to identify data types.

@since 1.2.0.0
-}
newtype Oid
  = Oid Word32
  deriving (Eq, Ord, Show)
