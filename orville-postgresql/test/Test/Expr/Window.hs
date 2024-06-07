module Test.Expr.Window
  ( windowTests
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import Test.Expr.TestSchema (barColumn)
import qualified Test.Property as Property

windowTests :: Property.Group
windowTests =
  Property.group
    "Expr - Window"
    [ prop_windowClauseSingleDef
    , prop_windowClauseMultiDef
    ]

prop_windowClauseSingleDef :: Property.NamedProperty
prop_windowClauseSingleDef =
  let
    windowDef = Expr.windowDefinition Nothing (Just partitionBy) (Just orderBy) (Just frameClause)
    windowDefName = Expr.fromIdentifier $ Expr.identifier "a"
    partitionBy = Expr.partitionBy (pure . Expr.valueExpression $ SqlValue.fromInt32 1)
    orderBy = Expr.orderByClause $ Expr.orderByColumnName barColumn Expr.ascendingOrder
    frameClause = Expr.frameClause (Just Expr.rowsFrameMode) Expr.unboundedPrecedingFrameStart (Just Expr.unboundedFollowingFrameEnd) (Just Expr.tiesFrameExclusion)
  in
    Property.singletonNamedProperty "window clause with single window definition containing a partition by, an order by, and a frame clause generates expected sql."
      . assertWindowEquals
        "WINDOW \"a\" AS (PARTITION BY $1 ORDER BY \"bar\" ASC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING EXCLUDE TIES)"
      . Expr.windowClause
      $ pure (windowDefName, windowDef)

prop_windowClauseMultiDef :: Property.NamedProperty
prop_windowClauseMultiDef =
  let
    windowDef1 = Expr.windowDefinition Nothing (Just partitionBy) Nothing Nothing
    windowDef1Name = Expr.fromIdentifier $ Expr.identifier "a"
    windowDef2 = Expr.windowDefinition (Just windowDef1Name) Nothing (Just orderBy) (Just frameClause)
    windowDef2Name = Expr.fromIdentifier $ Expr.identifier "b"
    partitionBy = Expr.partitionBy (pure . Expr.valueExpression $ SqlValue.fromInt32 1)
    orderBy = Expr.orderByClause $ Expr.orderByColumnName barColumn Expr.ascendingOrder
    frameClause = Expr.frameClause (Just Expr.rowsFrameMode) Expr.unboundedPrecedingFrameStart (Just Expr.unboundedFollowingFrameEnd) (Just Expr.tiesFrameExclusion)
  in
    Property.singletonNamedProperty "window clause with two window definitions, where the second \"copies\" the first,  generates expected sql."
      . assertWindowEquals
        "WINDOW \"a\" AS (PARTITION BY $1), \"b\" AS (\"a\" ORDER BY \"bar\" ASC ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING EXCLUDE TIES)"
      $ Expr.windowClause ((windowDef1Name, windowDef1) NE.:| pure (windowDef2Name, windowDef2))

assertWindowEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.WindowClause -> m ()
assertWindowEquals windowClauseStr windowClause =
  withFrozenCallStack $
    RawSql.toExampleBytes windowClause HH.=== B8.pack windowClauseStr
