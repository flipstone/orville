{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.ConstraintName
  ( ConstraintName,
    constraintName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ConstraintName
  = ConstraintName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

constraintName :: String -> ConstraintName
constraintName = ConstraintName . identifier
