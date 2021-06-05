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

import Database.Orville.PostgreSQL.Internal.Expr.Where.BooleanExpr
  ( BooleanExpr
  , booleanExprToSql
  )
import Database.Orville.PostgreSQL.Internal.RawSql
  ( RawSql
  , fromString
  )

newtype WhereClause =
  WhereClause RawSql

whereClauseToSql :: WhereClause -> RawSql
whereClauseToSql (WhereClause sql) = sql

whereClause :: BooleanExpr -> WhereClause
whereClause booleanExpr =
  WhereClause $
    fromString "WHERE " <> booleanExprToSql booleanExpr
