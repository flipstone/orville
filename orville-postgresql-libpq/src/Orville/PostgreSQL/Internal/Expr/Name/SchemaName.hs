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

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype SchemaName
  = SchemaName RawSql.RawSql
  deriving (RawSql.SqlExpression)

schemaName :: String -> SchemaName
schemaName =
  SchemaName . RawSql.fromString
