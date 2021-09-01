{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.SchemaName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.SchemaName
  ( SchemaName,
    schemaName,
    schemaNameFromIdentifier,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SchemaName
  = SchemaName Identifier
  deriving (RawSql.SqlExpression)

schemaName :: String -> SchemaName
schemaName =
  SchemaName . identifier

schemaNameFromIdentifier :: Identifier -> SchemaName
schemaNameFromIdentifier =
  SchemaName
