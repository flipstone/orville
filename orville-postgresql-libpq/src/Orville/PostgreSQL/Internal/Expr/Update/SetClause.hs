{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module    : Orville.PostgreSQL.Expr.Internal.Update.SetClause
Copyright : Flipstone Technology Partners 2021
License   : MIT
-}
module Orville.PostgreSQL.Internal.Expr.Update.SetClause
  ( SetClause,
    setColumn,
  )
where

import Orville.PostgreSQL.Internal.Expr.Name (ColumnName)
import qualified Orville.PostgreSQL.Internal.RawSql as RawSql
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

newtype SetClause
  = SetClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

setColumn :: ColumnName -> SqlValue.SqlValue -> SetClause
setColumn columnName value =
  SetClause $
    RawSql.toRawSql columnName
      <> RawSql.fromString "="
      <> RawSql.parameter value
