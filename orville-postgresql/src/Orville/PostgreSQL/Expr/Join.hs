{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024-2025
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
  , joinedTable
  , join
  , joining
  ) where

import Orville.PostgreSQL.Expr.TableReferenceList (TableReference)
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
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

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
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'JoinConstraint' from a given 'BooleanExpr' that specifies which rows in the JOIN are
   considering to match.

@since 1.1.0.0
-}
joinOnConstraint :: BooleanExpr -> JoinConstraint
joinOnConstraint booleanExpr =
  JoinConstraint $
    RawSql.fromString "ON " <> RawSql.toRawSql booleanExpr

{- | Constructs a 'TableReference' by creating a join expression from two existing table references.
  The result is an @n+m@ way join, where @n@ is the number of tables referenced in the first table
  reference expression (which itself may be a join) and @m@ is the number of tables referenced by
  the second table reference expression.

  See also: 'join', 'joining'

@since 1.1.0.0
-}
joinedTable ::
  TableReference ->
  JoinType ->
  TableReference ->
  JoinConstraint ->
  TableReference
joinedTable tableRefA joinType tableRefB joinOn =
  RawSql.unsafeFromRawSql $
    RawSql.toRawSql tableRefA
      <> RawSql.space
      <> RawSql.toRawSql joinType
      <> RawSql.space
      <> RawSql.toRawSql tableRefB
      <> RawSql.space
      <> RawSql.toRawSql joinOn

{- | A flipped version of 'joinedTable' that allows joined tables to be constructed in a syntax more
  similar to SQL. For example:

  @@
    fooTableRef
      & join leftJoinType barTableRef someJoinCondition
      & join leftJoinType bazTableRef someOtherJoinCondition
  @@

  See also: 'joining', 'joinedTable'

  @since 1.1.0.0
-}
join ::
  JoinType ->
  TableReference ->
  JoinConstraint ->
  TableReference ->
  TableReference
join joinType tableRefB joinOn tableRefA =
  joinedTable tableRefA joinType tableRefB joinOn

{- | A convenience function for constructing joins by tracking the tables to be joined in a list
  instead of using the '(&)' operator. The tables will be joined in a left associative manner,
  matching the associativity of the analogous SQL expression.

  @@
    joining fooTableRef
      [ join leftJoinType barTableRef someJoinCondition
      , join leftJoinType bazTableRef someOtherJoinCondition
      ]
  @@

  See also: 'join', 'joinedTable'

  @since 1.1.0.0
-}
joining :: TableReference -> [TableReference -> TableReference] -> TableReference
joining tableRef joinList =
  foldr (flip (.)) id joinList tableRef
