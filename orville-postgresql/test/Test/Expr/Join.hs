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
    "Expr - Join and FromItemExpr"
    [ prop_leftJoinOnTrue
    ]

prop_leftJoinOnTrue :: Property.NamedProperty
prop_leftJoinOnTrue =
  Property.singletonNamedProperty "joinExpr with left join and trivial on clause generates expected sql"
    $ assertJoinEquals
      "LEFT JOIN \"foo\" ON TRUE"
    $ Expr.joinExpr Expr.leftJoinType fooFromItem joinOnTrue

assertJoinEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.JoinExpr -> m ()
assertJoinEquals mbJoinStr joinExpr =
  withFrozenCallStack $
    RawSql.toExampleBytes joinExpr HH.=== B8.pack mbJoinStr

joinOnTrue :: Expr.JoinConstraint
joinOnTrue =
  Expr.joinOnConstraint $ Expr.literalBooleanExpr True

fooFromItem :: Expr.FromItemExpr
fooFromItem = Expr.tableFromItem . Expr.qualifyTable Nothing $ Expr.tableName "foo"
