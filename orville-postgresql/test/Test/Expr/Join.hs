module Test.Expr.Join
  ( joinTests
  )
where

import qualified Data.ByteString.Char8 as B8
import Data.Function ((&))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql

import qualified Test.Property as Property

joinTests :: Tasty.TestTree
joinTests =
  Tasty.testGroup
    "Expr - Join"
    [ TastyHH.testProperty "joinedTable produces a table ref that joins two tables together" prop_joinedTable
    , TastyHH.testProperty "join allows 'joinedTable' to be used more naturally" prop_join
    , TastyHH.testProperty "joining allows 'join' to be used without the user needing to use (&)" prop_joining
    ]

prop_joinedTable :: HH.Property
prop_joinedTable =
  Property.singletonProperty $
    assertTableRefEquals
      "\"foo\" LEFT JOIN \"bar\" ON TRUE"
      (Expr.joinedTable fooTableRef Expr.leftJoinType barTableRef joinOnTrue)

prop_join :: HH.Property
prop_join =
  Property.singletonProperty $
    let
      joinedTableRef =
        fooTableRef
          & Expr.join Expr.leftJoinType barTableRef joinOnTrue
          & Expr.join Expr.leftJoinType bazTableRef joinOnTrue
    in
      assertTableRefEquals
        "\"foo\" LEFT JOIN \"bar\" ON TRUE LEFT JOIN \"baz\" ON TRUE"
        joinedTableRef

prop_joining :: HH.Property
prop_joining =
  Property.singletonProperty $
    let
      joinedTableRef =
        Expr.joining
          fooTableRef
          [ Expr.join Expr.leftJoinType barTableRef joinOnTrue
          , Expr.join Expr.leftJoinType bazTableRef joinOnTrue
          ]
    in
      assertTableRefEquals
        "\"foo\" LEFT JOIN \"bar\" ON TRUE LEFT JOIN \"baz\" ON TRUE"
        joinedTableRef

assertTableRefEquals ::
  (HH.MonadTest m, HasCallStack) => String -> Expr.TableReference -> m ()
assertTableRefEquals expectedString tableRef =
  withFrozenCallStack $
    RawSql.toExampleBytes tableRef === B8.pack expectedString

joinOnTrue :: Expr.JoinConstraint
joinOnTrue =
  Expr.joinOnConstraint $ Expr.literalBooleanExpr True

fooTableRef :: Expr.TableReference
fooTableRef = Expr.tableNameReference (Expr.unqualified $ Expr.tableName "foo") Nothing

barTableRef :: Expr.TableReference
barTableRef = Expr.tableNameReference (Expr.unqualified $ Expr.tableName "bar") Nothing

bazTableRef :: Expr.TableReference
bazTableRef = Expr.tableNameReference (Expr.unqualified $ Expr.tableName "baz") Nothing
