{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.SchemaName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.SchemaName
  ( SchemaName,
    schemaName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SchemaName
  = SchemaName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

schemaName :: String -> SchemaName
schemaName =
  SchemaName . identifier
