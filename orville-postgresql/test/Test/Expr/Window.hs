module Test.Expr.Window
  ( windowTests
  )
where

import qualified Data.ByteString.Char8 as B8
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (barColumn)
import qualified Test.Property as Property

windowTests :: Tasty.TestTree
windowTests =
  Tasty.testGroup
    "Expr - Window"
    [ TastyHH.testProperty "window clause with single window definition containing a partition by, an order by, and a frame clause generates expected sql." prop_windowClauseSingleDef
    , TastyHH.testProperty "window clause with two window definitions, where the second \"copies\" the first,  generates expected sql." prop_windowClauseMultiDef
    ]

prop_windowClauseSingleDef :: HH.Property
prop_windowClauseSingleDef =
  Property.singletonProperty $
    let
      windowDef = Expr.windowDefinition Nothing (Just partitionBy) (Just orderBy) (Just frameClause)
      windowDefName = Expr.fromIdentifier $ Expr.identifier "a"
      partitionBy = Expr.partitionBy (pure . Expr.valueExpression $ SqlValue.fromInt32 1)
      orderBy = Expr.orderByClause $ Expr.orderByColumnName barColumn Expr.ascendingOrder
      frameClause = Expr.frameClause (Just Expr.rowsFrameMode) Expr.unboundedPrecedingFrameStart (Just Expr.unboundedFollowingFrameEnd) (Just Expr.tiesFrameExclusion)
    in
      assertWindowEquals
        "WINDOW \"a\" AS (PARTITION BY $1 ORDER BY \"bar\" ASC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING EXCLUDE TIES)"
        (Expr.windowClause $ Expr.namedWindowDefinition windowDefName windowDef)

prop_windowClauseMultiDef :: HH.Property
prop_windowClauseMultiDef =
  Property.singletonProperty $
    let
      windowDef1 = Expr.windowDefinition Nothing (Just partitionBy) Nothing Nothing
      windowDef1Name = Expr.fromIdentifier $ Expr.identifier "a"
      windowDef2 = Expr.windowDefinition (Just windowDef1Name) Nothing (Just orderBy) (Just frameClause)
      windowDef2Name = Expr.fromIdentifier $ Expr.identifier "b"
      partitionBy = Expr.partitionBy (pure . Expr.valueExpression $ SqlValue.fromInt32 1)
      orderBy = Expr.orderByClause $ Expr.orderByColumnName barColumn Expr.ascendingOrder
      frameClause = Expr.frameClause (Just Expr.rowsFrameMode) Expr.unboundedPrecedingFrameStart (Just Expr.unboundedFollowingFrameEnd) (Just Expr.tiesFrameExclusion)
    in
      assertWindowEquals
        "WINDOW \"a\" AS (PARTITION BY $1), \"b\" AS (\"a\" ORDER BY \"bar\" ASC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING EXCLUDE TIES)"
        (Expr.windowClause $ Expr.namedWindowDefinition windowDef1Name windowDef1 <> Expr.namedWindowDefinition windowDef2Name windowDef2)

assertWindowEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.WindowClause -> m ()
assertWindowEquals windowClauseStr windowClause =
  withFrozenCallStack $
    RawSql.toExampleBytes windowClause HH.=== B8.pack windowClauseStr
