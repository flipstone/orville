{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.SequenceName
  ( SequenceName,
    sequenceName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SequenceName
  = SequenceName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

sequenceName :: String -> SequenceName
sequenceName =
  SequenceName . identifier
