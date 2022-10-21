{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.SequenceName
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.SequenceName
  ( SequenceName,
    sequenceName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SequenceName
  = SequenceName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

sequenceName :: String -> SequenceName
sequenceName =
  SequenceName . identifier
