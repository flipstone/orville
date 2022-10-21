{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.Qualified
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.Qualified
  ( Qualified,
    qualified,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Internal.Expr.Name.SchemaName (SchemaName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype Qualified name
  = Qualified RawSql.RawSql
  deriving (RawSql.SqlExpression)

qualified ::
  IdentifierExpression name =>
  Maybe SchemaName ->
  name ->
  Qualified name
qualified mbSchemaName unqualifiedName =
  case mbSchemaName of
    Nothing ->
      Qualified . RawSql.toRawSql . toIdentifier $ unqualifiedName
    Just schemaName ->
      Qualified
        ( RawSql.toRawSql schemaName
            <> RawSql.dot
            <> RawSql.toRawSql (toIdentifier unqualifiedName)
        )
