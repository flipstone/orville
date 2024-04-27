{-# OPTIONS_GHC -Wno-missing-import-lists #-}

{- |
Copyright : Flipstone Technology Partners 2023-2024
License   : MIT
Stability : Stable

You can import "Orville.PostgreSQL.Marshall" to get access to all the functions
related to marshalling Haskell values to and from their SQL representations.
This includes a number of lower-level items not exported by
"Orville.PostgreSQL" that give you more control (and therefore responsibility)
over how the marshalling is performed.

@since 1.0.0.0
-}
module Orville.PostgreSQL.Marshall
  ( module Orville.PostgreSQL.Marshall.SqlMarshaller
  , module Orville.PostgreSQL.Marshall.FieldDefinition
  , module Orville.PostgreSQL.Marshall.DefaultValue
  , module Orville.PostgreSQL.Marshall.SyntheticField
  , module Orville.PostgreSQL.Marshall.MarshallError
  , module Orville.PostgreSQL.Marshall.SqlType
  , module Orville.PostgreSQL.Marshall.AliasName
  , module Orville.PostgreSQL.Marshall.SqlComparable
  )
where

-- Note: we list the re-exports explicity above to control the order that they
-- appear in the generated haddock documentation.

import Orville.PostgreSQL.Marshall.AliasName
import Orville.PostgreSQL.Marshall.DefaultValue
import Orville.PostgreSQL.Marshall.FieldDefinition
import Orville.PostgreSQL.Marshall.MarshallError
import Orville.PostgreSQL.Marshall.SqlComparable
import Orville.PostgreSQL.Marshall.SqlMarshaller
import Orville.PostgreSQL.Marshall.SqlType
import Orville.PostgreSQL.Marshall.SyntheticField
