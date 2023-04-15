{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.TableName
  ( TableName,
    tableName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype TableName
  = TableName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

tableName :: String -> TableName
tableName =
  TableName . identifier
