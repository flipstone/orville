{-|
Module    : Database.Orville.PostgreSQL.Expr.Where.WhereClause
Copyright : Flipstone Technology Partners 2016-2021
License   : MIT
-}

module Database.Orville.PostgreSQL.Internal.Expr.Where.WhereClause
  ( WhereClause
  , whereClauseToSql
  , whereClause
  ) where

import           Database.Orville.PostgreSQL.Internal.Expr.Where.BooleanExpr (BooleanExpr, booleanExprToSql)
import qualified Database.Orville.PostgreSQL.Internal.RawSql                 as RawSql

newtype WhereClause =
  WhereClause RawSql.RawSql

whereClauseToSql :: WhereClause -> RawSql.RawSql
whereClauseToSql (WhereClause sql) = sql

whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    RawSql.fromString "WHERE " <> booleanExprToSql booleanExpr
