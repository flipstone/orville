{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.ConstraintName
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.ConstraintName
  ( ConstraintName,
    constraintName,
    constraintNameFromIdentifier,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype ConstraintName
  = ConstraintName Identifier
  deriving (RawSql.SqlExpression)

constraintName :: String -> ConstraintName
constraintName = ConstraintName . identifier

constraintNameFromIdentifier :: Identifier -> ConstraintName
constraintNameFromIdentifier = ConstraintName
