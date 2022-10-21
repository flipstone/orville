{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.FunctionName
Copyright : Flipstone Technology Partners 2022
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.FunctionName
  ( FunctionName,
    functionName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype FunctionName
  = FunctionName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

functionName :: String -> FunctionName
functionName =
  FunctionName . identifier
