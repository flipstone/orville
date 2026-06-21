module Test.Expr.Math
  ( mathTests
  )
where

import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import Data.Int (Int32)
import GHC.Stack (withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.SqlValue as SqlValue

mathTests :: Orville.ConnectionPool -> Tasty.TestTree
mathTests pool =
  Tasty.testGroup
    "Expr - Math"
    [ TastyHH.testProperty "plus" (prop_plus pool)
    , TastyHH.testProperty "minus" (prop_minus pool)
    , TastyHH.testProperty "multiply" (prop_multiply pool)
    , TastyHH.testProperty "divide" (prop_divide pool)
    , TastyHH.testProperty "exponentiate" (prop_exponentiate pool)
    , TastyHH.testProperty "bitwiseAnd" (prop_bitwiseAnd pool)
    , TastyHH.testProperty "bitwiseOr" (prop_bitwiseOr pool)
    , TastyHH.testProperty "bitwiseXor" (prop_bitwiseXor pool)
    , TastyHH.testProperty "bitwiseShiftLeft" (prop_bitwiseShiftLeft pool)
    , TastyHH.testProperty "bitwiseShiftRight" (prop_bitwiseShiftRight pool)
    ]

prop_plus :: Orville.ConnectionPool -> HH.Property
prop_plus pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.plus
          (int32Expression n)
          (int32Expression m)

    result === (n + m)

prop_minus :: Orville.ConnectionPool -> HH.Property
prop_minus pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.minus
          (int32Expression n)
          (int32Expression m)

    result === (n - m)

prop_multiply :: Orville.ConnectionPool -> HH.Property
prop_multiply pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.multiply
          (int32Expression n)
          (int32Expression m)

    result === (n * m)

prop_divide :: Orville.ConnectionPool -> HH.Property
prop_divide pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.filter (/= 0) (Gen.integral (Range.linearFrom 0 (-100) 100)))

    result <-
      evaluateIntegerExpression pool $
        Expr.divide
          (int32Expression n)
          (int32Expression m)

    result === (n `quot` m)

prop_exponentiate :: Orville.ConnectionPool -> HH.Property
prop_exponentiate pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 10))
    m <- HH.forAll (Gen.integral (Range.linear 0 10))

    result <-
      evaluateIntegerExpression pool $
        Expr.exponentiate
          (int32Expression n)
          (int32Expression m)

    result === (n ^ m)

prop_bitwiseAnd :: Orville.ConnectionPool -> HH.Property
prop_bitwiseAnd pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseAnd
          (int32Expression n)
          (int32Expression m)

    result === (n .&. m)

prop_bitwiseOr :: Orville.ConnectionPool -> HH.Property
prop_bitwiseOr pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseOr
          (int32Expression n)
          (int32Expression m)

    result === (n .|. m)

prop_bitwiseXor :: Orville.ConnectionPool -> HH.Property
prop_bitwiseXor pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseXor
          (int32Expression n)
          (int32Expression m)

    result === Bits.xor n m

prop_bitwiseShiftLeft :: Orville.ConnectionPool -> HH.Property
prop_bitwiseShiftLeft pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 64))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseShiftLeft
          (int32Expression n)
          (intExpression m)

    result === Bits.shiftL n (m `mod` 32)

prop_bitwiseShiftRight :: Orville.ConnectionPool -> HH.Property
prop_bitwiseShiftRight pool =
  HH.property $ do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 64))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseShiftRight
          (int32Expression n)
          (intExpression m)

    result === Bits.shiftR n (m `mod` 32)

int32Expression :: Int32 -> Expr.ValueExpression
int32Expression n =
  Expr.cast (Expr.valueExpression (SqlValue.fromInt32 n)) Expr.int

intExpression :: Int -> Expr.ValueExpression
intExpression n =
  Expr.cast (Expr.valueExpression (SqlValue.fromInt n)) Expr.int

evaluateIntegerExpression ::
  Orville.ConnectionPool ->
  Expr.ValueExpression ->
  HH.PropertyT IO Int32
evaluateIntegerExpression pool expression = do
  let
    sql =
      Expr.queryExpr
        (Expr.selectClause (Expr.selectExpr Nothing))
        ( Expr.selectDerivedColumns
            [ Expr.deriveColumnAs expression (Expr.columnName "result")
            ]
        )
        Nothing

    marshaller =
      Orville.annotateSqlMarshallerEmptyAnnotation $
        Orville.marshallField id (Orville.integerField "result")

  results <-
    HH.evalIO $
      Orville.runOrville pool $
        Orville.executeAndDecode Orville.SelectQuery sql marshaller

  case results of
    [result] ->
      pure result
    _ ->
      withFrozenCallStack $ do
        HH.annotate $ "Expected exactly 1 row in result, but got the rows: " <> show results
        HH.failure
