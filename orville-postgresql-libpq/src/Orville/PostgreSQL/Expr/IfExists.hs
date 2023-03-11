{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Orville.PostgreSQL.Expr.IfExists
  ( IfExists,
    ifExists,
  )
where

import qualified Orville.PostgreSQL.Internal.RawSql as RawSql

newtype IfExists
  = IfExists RawSql.RawSql
  deriving (RawSql.SqlExpression)

ifExists :: IfExists
ifExists =
  IfExists $ RawSql.fromString "IF EXISTS"
