module Test.Expr.ConditionalExpr
  ( conditionalTests
  )
where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified Hedgehog as HH
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

import qualified Test.Property as Property

conditionalTests :: Tasty.TestTree
conditionalTests =
  Tasty.testGroup
    "Expr - Conditional"
    [ TastyHH.testProperty "caseExpr with a trivial WHEN and no ELSE generates expected sql." prop_caseWithNoElse
    , TastyHH.testProperty "caseExpr with a simple WHEN and simple ELSE generates expected sql." prop_caseWithElse
    , TastyHH.testProperty "caseExpr with multiple WHENs and a simple ELSE generates expected sql." prop_caseMultipleWhenWithElse
    ]

prop_caseWithNoElse :: HH.Property
prop_caseWithNoElse =
  Property.singletonProperty $
    let
      whenTrueThen1 =
        Expr.whenExpr (Expr.literalBooleanExpr True) (Expr.valueExpression $ SqlValue.fromInt32 1)
      caseExpr = Expr.caseExpr (pure whenTrueThen1) Nothing
    in
      assertConditionalEquals
        "CASE WHEN TRUE THEN $1 END"
        caseExpr

prop_caseWithElse :: HH.Property
prop_caseWithElse =
  Property.singletonProperty $
    let
      firstEqualsSecond =
        Expr.equals (Expr.valueExpression $ SqlValue.fromInt32 1) (Expr.valueExpression $ SqlValue.fromInt32 1)
      whenExpr =
        Expr.whenExpr firstEqualsSecond (Expr.valueExpression $ SqlValue.fromInt32 1)
      caseExpr = Expr.caseExpr (pure whenExpr) (Just . Expr.valueExpression $ SqlValue.fromInt32 1)
    in
      assertConditionalEquals
        "CASE WHEN ($1) = ($2) THEN $3 ELSE $4 END"
        caseExpr

prop_caseMultipleWhenWithElse :: HH.Property
prop_caseMultipleWhenWithElse =
  Property.singletonProperty $
    let
      firstVal = Expr.valueExpression $ SqlValue.fromInt32 1
      secondVal = Expr.valueExpression $ SqlValue.fromInt32 1
      firstGreaterSecond =
        Expr.greaterThan firstVal secondVal
      firstLessSecond =
        Expr.lessThan firstVal secondVal
      whenGreaterExpr =
        Expr.whenExpr firstGreaterSecond (Expr.valueExpression $ SqlValue.fromInt32 1)
      whenLessExpr =
        Expr.whenExpr firstLessSecond (Expr.valueExpression $ SqlValue.fromInt32 1)
      caseExpr = Expr.caseExpr (whenGreaterExpr NE.:| [whenLessExpr]) (Just . Expr.valueExpression $ SqlValue.fromInt32 1)
    in
      assertConditionalEquals
        "CASE WHEN ($1) > ($2) THEN $3 WHEN ($4) < ($5) THEN $6 ELSE $7 END"
        caseExpr

assertConditionalEquals :: (HH.MonadTest m, HasCallStack) => String -> Expr.ValueExpression -> m ()
assertConditionalEquals valueExpressionStr valueExpr =
  withFrozenCallStack $
    RawSql.toExampleBytes valueExpr HH.=== B8.pack valueExpressionStr
