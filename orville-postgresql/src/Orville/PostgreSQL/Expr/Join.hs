{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Join
  ( JoinType
  , innerJoinType
  , leftJoinType
  , rightJoinType
  , fullJoinType
  , leftLateralJoinType
  , innerLateralJoinType
  , JoinConstraint
  , joinOnConstraint
  , JoinExpr
  , joinExpr
  , appendJoinFromItem
  ) where

import Orville.PostgreSQL.Expr.FromItemExpr (FromItemExpr)
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

@since 1.1.0.0
-}
newtype JoinType = JoinType RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Constructs a 'JoinType' for an INNER JOIN.

@since 1.1.0.0
-}
innerJoinType :: JoinType
innerJoinType = JoinType $ RawSql.fromString "INNER JOIN"

{- | Constructs a 'JoinType' for a LEFT JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 1.1.0.0
-}
leftJoinType :: JoinType
leftJoinType = JoinType $ RawSql.fromString "LEFT JOIN"

{- | Constructs a 'JoinType' for a RIGHT JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 1.1.0.0
-}
rightJoinType :: JoinType
rightJoinType = JoinType $ RawSql.fromString "RIGHT JOIN"

{- | Constructs a 'JoinType' for a FULL JOIN.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 1.1.0.0
-}
fullJoinType :: JoinType
fullJoinType = JoinType $ RawSql.fromString "FULL JOIN"

{- | Constructs a 'JoinType' for a LEFT JOIN LATERAL.

  Note that the documentation at https://www.postgresql.org/docs/15/sql-select.html describes the
  keyword OUTER as optional, and the sql generated from here omits it for brevity of query.

@since 1.1.0.0
-}
leftLateralJoinType :: JoinType
leftLateralJoinType = JoinType $ RawSql.fromString "LEFT JOIN LATERAL"

{- | Constructs a 'JoinType' for an INNER JOIN.

@since 1.1.0.0
-}
innerLateralJoinType :: JoinType
innerLateralJoinType = JoinType $ RawSql.fromString "JOIN LATERAL"

{- | Representation of the "ON" part of a JOIN in sql.

@since 1.1.0.0
-}
newtype JoinConstraint = JoinConstraint RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Constructs a 'JoinConstraint' from a given 'BooleanExpr' that specifies which rows in the JOIN are
   considering to match.

@since 1.1.0.0
-}
joinOnConstraint :: BooleanExpr -> JoinConstraint
joinOnConstraint booleanExpr =
  JoinConstraint $
    RawSql.fromString "ON " <> RawSql.toRawSql booleanExpr

{- | Representation of @JOIN@, modifiers to it, such as @LEFT@, and any conditions for the @JOIN@.

@since 1.1.0.0
-}
newtype JoinExpr = JoinExpr RawSql.RawSql
  deriving (RawSql.SqlExpression)

{- | Build a 'JoinExpr' with the given options. It is up to the caller to ensure the 'FromItemExpr' is
   appropriately aliased.

@since 1.1.0.0
-}
joinExpr ::
  JoinType ->
  FromItemExpr ->
  JoinConstraint ->
  JoinExpr
joinExpr joinType tableRefList joinOn =
  JoinExpr $
    RawSql.toRawSql joinType
      <> RawSql.space
      <> RawSql.toRawSql tableRefList
      <> RawSql.space
      <> RawSql.toRawSql joinOn

{- | Add 'JoinExpr's to a given 'FromItemExpr'. Resuling in an n+m way join, where n is the
   number of tables referenced by the given 'FromItemExpr', and m is the number of joins in
   the list of 'JoinExpr's.

@since 1.1.0.0
-}
appendJoinFromItem :: FromItemExpr -> [JoinExpr] -> FromItemExpr
appendJoinFromItem item [] = item
appendJoinFromItem item joins =
  RawSql.unsafeFromRawSql
    . RawSql.intercalate
      RawSql.space
    $ (RawSql.toRawSql item) : (fmap RawSql.toRawSql joins)
