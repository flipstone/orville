{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
-}
module Orville.PostgreSQL.Expr.Join
  ( JoinType
  , innerJoinType
  , leftJoinType
  , rightJoinType
  , fullJoinType
  , JoinOnClause
  , joinOnClause
  ) where

import Orville.PostgreSQL.Expr.WhereClause (BooleanExpr)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

{- | Representation of what kind of 'JOIN' to perform.

  From the documentation at https://www.postgresql.org/docs/15/sql-select.html there are 4 basic
  types of join.

  - Inner join
    Use 'innerJoinType' for constructing
  - Left join
    Sometimes called a left "outer" join. Use 'leftJoinType' for construction.
  - Right join
    Sometimes called a right "outer" join. use 'rightJoinType' for construction.
  - Full join
    Sometimes called a full "outer" join. Use 'fullJoinType' for construction.

@since 0.10.0.0
-}
newtype JoinType = JoinType RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Constructs a 'JoinType' for an INNER JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword INNER as optional, and the sql generated from here omits it for brevity of query.

@since 0.10.0.0
-}
innerJoinType :: JoinType
innerJoinType = JoinType $ RawSql.fromString "JOIN"

{- | Constructs a 'JoinType' for a LEFT JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 0.10.0.0
-}
leftJoinType :: JoinType
leftJoinType = JoinType $ RawSql.fromString "LEFT JOIN"

{- | Constructs a 'JoinType' for a RIGHT JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 0.10.0.0
-}
rightJoinType :: JoinType
rightJoinType = JoinType $ RawSql.fromString "RIGHT JOIN"

{- | Constructs a 'JoinType' for a FULL JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 0.10.0.0
-}
fullJoinType :: JoinType
fullJoinType = JoinType $ RawSql.fromString "FULL JOIN"

{- | Representation of the "ON" part of a JOIN in sql.

@since 0.10.0.0
-}
newtype JoinOnClause = JoinOnClause RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Constructs a 'JoinOnClause' from a given 'BooleanExpr' that specifies which rows in the JOIN are
   considering to match.

@since 0.10.0.0
-}
joinOnClause :: BooleanExpr -> JoinOnClause
joinOnClause booleanExpr =
  JoinOnClause $
    RawSql.fromString "ON " <> RawSql.toRawSql booleanExpr
