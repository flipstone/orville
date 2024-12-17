{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Orphans () where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE

import qualified Orville.PostgreSQL.Marshall as Marshall
import qualified Orville.PostgreSQL.Raw.PgTextFormatValue as PgTextFormatValue
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

instance Show SqlValue.SqlValue where
  show =
    SqlValue.foldSqlValue
      (B8.unpack . PgTextFormatValue.toByteString)
      (\vals -> "(" <> List.intercalate ", " (NE.toList vals) <> ")")
      "NULL"

deriving instance Show Marshall.FieldIdentityGeneration
deriving instance Enum Marshall.FieldIdentityGeneration
deriving instance Bounded Marshall.FieldIdentityGeneration
