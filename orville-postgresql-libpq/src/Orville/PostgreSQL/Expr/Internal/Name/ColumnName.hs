{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}
module Orville.PostgreSQL.Expr.Internal.Name.ColumnName
  ( ColumnName,
    columnName,
  )
where

import Orville.PostgreSQL.Expr.Internal.Name.Identifier (Identifier, IdentifierExpression, identifier)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

newtype ColumnName
  = ColumnName Identifier
  deriving (RawSql.SqlExpression, IdentifierExpression)

columnName :: String -> ColumnName
columnName = ColumnName . identifier
