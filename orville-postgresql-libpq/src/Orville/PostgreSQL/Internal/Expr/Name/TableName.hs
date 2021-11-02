{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Name.TableName
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Name.TableName
  ( TableName,
    tableName,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableName
  = TableName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

tableName :: String -> TableName
tableName =
  TableName . identifier
