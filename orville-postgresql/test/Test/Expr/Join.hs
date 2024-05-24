module Test.Expr.Join
  ( joinTests
  )
where

import qualified Data.ByteString.Char8 as B8
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

joinTests :: Property.Group
joinTests =
  Property.group
    "Expr - Join"
    [ prop_leftJoinOnTrue
    , prop_appendJoin
    ]

prop_leftJoinOnTrue :: Property.NamedProperty
prop_leftJoinOnTrue =
  Property.singletonNamedProperty "joinExpr with left join and trivial on clause generates expected sql"
    $ assertJoinEquals
      "LEFT JOIN \"foo\" ON TRUE"
    $ Expr.joinExpr Expr.leftJoinType fooFromItem joinOnTrue

prop_appendJoin :: Property.NamedProperty
prop_appendJoin =
  Property.singletonNamedProperty "appending a single joinExpr with left join lateral and trivial on clause to another FromItemExpr generates expected sql"
    $ assertFromItemEquals
      "\"bar\" LEFT JOIN LATERAL \"foo\" ON TRUE"
      . Expr.appendJoinFromItem barFromItem
      . pure
    $ Expr.joinExpr Expr.leftLateralJoinType fooFromItem joinOnTrue

assertJoinEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.JoinExpr -> m ()
assertJoinEquals mbJoinStr joinExpr =
  withFrozenCallStack $
    RawSql.toExampleBytes joinExpr HH.=== B8.pack mbJoinStr

assertFromItemEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.FromItemExpr -> m ()
assertFromItemEquals mbFromItemStr fromItemExpr =
  withFrozenCallStack $
    RawSql.toExampleBytes fromItemExpr HH.=== B8.pack mbFromItemStr

joinOnTrue :: Expr.JoinConstraint
joinOnTrue =
  Expr.joinOnConstraint $ Expr.literalBooleanExpr True

fooFromItem :: Expr.FromItemExpr
fooFromItem = Expr.tableFromItem . Expr.qualifyTable Nothing $ Expr.tableName "foo"

barFromItem :: Expr.FromItemExpr
barFromItem = Expr.tableFromItem . Expr.qualifyTable Nothing $ Expr.tableName "bar"
