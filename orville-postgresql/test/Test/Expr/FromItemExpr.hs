module Test.Expr.FromItemExpr
  ( fromItemExprTests
  ) where

import qualified Data.ByteString.Char8 as B8
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

fromItemExprTests :: Property.Group
fromItemExprTests =
  Property.group
    "Expr - FromItemExpr"
    [ prop_appendJoin
    , prop_cartesianProduct
    , prop_appendFromItemIdentity
    , prop_appendFromItemAssociativity
    ]

prop_appendJoin :: Property.NamedProperty
prop_appendJoin =
  Property.singletonNamedProperty "appending a single joinExpr with left join lateral and trivial on clause to another FromItemExpr generates expected sql"
    $ assertFromItemEquals
      "\"bar\" LEFT JOIN LATERAL \"foo\" ON TRUE"
      . Expr.appendJoinFromItem barFromItem
      . pure
    $ Expr.joinExpr Expr.leftLateralJoinType fooFromItem joinOnTrue

prop_cartesianProduct :: Property.NamedProperty
prop_cartesianProduct =
  Property.singletonNamedProperty "appending two FromItemExprs results in a cartesian product FromItemExpr" $
    assertFromItemEquals
      "\"foo\", \"bar\""
      (fooFromItem <> barFromItem)

prop_appendFromItemIdentity :: Property.NamedProperty
prop_appendFromItemIdentity =
  Property.singletonNamedProperty "FromItemExpr Monoid identity" $ do
    let
      expected = "\"foo\""
    assertFromItemEquals expected (mempty <> fooFromItem)
    assertFromItemEquals expected (fooFromItem <> mempty)

prop_appendFromItemAssociativity :: Property.NamedProperty
prop_appendFromItemAssociativity =
  Property.singletonNamedProperty "FromItemExpr Semigroup associativity" $ do
    let
      expected = "\"bar\", \"foo\", \"bar\""
    assertFromItemEquals expected (barFromItem <> (fooFromItem <> barFromItem))
    assertFromItemEquals expected ((barFromItem <> fooFromItem) <> barFromItem)

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
