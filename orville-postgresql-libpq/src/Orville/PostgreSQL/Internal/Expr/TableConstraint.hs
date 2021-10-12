{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Internal.Expr.TableConstraint
  ( TableConstraint,
    uniqueConstraint,
  )
where

import Data.List.NonEmpty (NonEmpty)

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype TableConstraint
  = TableConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

uniqueConstraint :: NonEmpty ColumnName -> TableConstraint
uniqueConstraint columnNames =
  TableConstraint $
    RawSql.fromString "UNIQUE "
      <> RawSql.leftParen
      <> RawSql.intercalate RawSql.comma columnNames
      <> RawSql.rightParen
