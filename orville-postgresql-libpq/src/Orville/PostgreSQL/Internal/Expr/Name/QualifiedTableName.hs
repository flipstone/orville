{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.QualifiedTableName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.QualifiedTableName
  ( QualifiedTableName,
    qualifiedTableName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.SchemaName (SchemaName)
import Orville.PostgreSQL.Internal.Expr.Name.TableName (TableName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype QualifiedTableName
  = QualifiedTableName RawSql.RawSql
  deriving (RawSql.SqlExpression)

qualifiedTableName :: Maybe SchemaName -> TableName -> QualifiedTableName
qualifiedTableName mbSchemaName tableName =
  case mbSchemaName of
    Nothing ->
      QualifiedTableName (RawSql.toRawSql tableName)
    Just schemaName ->
      QualifiedTableName
        ( RawSql.toRawSql schemaName
            <> RawSql.dot
            <> RawSql.toRawSql tableName
        )
