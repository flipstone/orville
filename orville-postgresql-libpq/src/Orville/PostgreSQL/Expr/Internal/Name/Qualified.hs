{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.Qualified
  ( Qualified,
    qualified,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (IdentifierExpression (toIdentifier))
import Orville.PostgreSQL.Expr.Internal.Name.SchemaName (SchemaName)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

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
