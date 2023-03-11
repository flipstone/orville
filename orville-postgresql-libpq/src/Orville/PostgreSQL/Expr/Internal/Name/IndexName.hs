{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Internal.Name.IndexName
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.IndexName
  ( IndexName,
    indexName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype IndexName
  = IndexName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

indexName :: String -> IndexName
indexName = IndexName . identifier
