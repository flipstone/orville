{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.SchemaName
  ( SchemaName,
    schemaName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype SchemaName
  = SchemaName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

schemaName :: String -> SchemaName
schemaName =
  SchemaName . identifier
