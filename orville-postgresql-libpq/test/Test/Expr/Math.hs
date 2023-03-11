module Test.Expr.Math
  ( mathTests,
  )
where

import Data.Bits ((.&.), (.|.))
import qualified Data.Bits as Bits
import Data.Int (Int32)
import qualified Data.Pool as Pool
import GHC.Stack (withFrozenCallStack)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Orville.PostgreSQL as Orville
import qualified Orville.PostgreSQL.Connection as Conn
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Internal.SqlValue as SqlValue

import qualified Test.Property as Property

mathTests :: Pool.Pool Conn.Connection -> Property.Group
mathTests pool =
  Property.group
    "Expr - Math"
    [ prop_plus pool
    , prop_minus pool
    , prop_multiply pool
    , prop_divide pool
    , prop_exponentiate pool
    , prop_bitwiseAnd pool
    , prop_bitwiseOr pool
    , prop_bitwiseXor pool
    , prop_bitwiseShiftLeft pool
    , prop_bitwiseShiftRight pool
    ]

prop_plus :: Property.NamedDBProperty
prop_plus =
  Property.namedDBProperty "plus" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.plus
          (int32Expression n)
          (int32Expression m)

    result === (n + m)

prop_minus :: Property.NamedDBProperty
prop_minus =
  Property.namedDBProperty "minus" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.minus
          (int32Expression n)
          (int32Expression m)

    result === (n - m)

prop_multiply :: Property.NamedDBProperty
prop_multiply =
  Property.namedDBProperty "multiply" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))

    result <-
      evaluateIntegerExpression pool $
        Expr.multiply
          (int32Expression n)
          (int32Expression m)

    result === (n * m)

prop_divide :: Property.NamedDBProperty
prop_divide =
  Property.namedDBProperty "divide" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linearFrom 0 (-100) 100))
    m <- HH.forAll (Gen.filter (/= 0) (Gen.integral (Range.linearFrom 0 (-100) 100)))

    result <-
      evaluateIntegerExpression pool $
        Expr.divide
          (int32Expression n)
          (int32Expression m)

    result === (n `quot` m)

prop_exponentiate :: Property.NamedDBProperty
prop_exponentiate =
  Property.namedDBProperty "exponentiate" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linear 0 10))
    m <- HH.forAll (Gen.integral (Range.linear 0 10))

    result <-
      evaluateIntegerExpression pool $
        Expr.exponentiate
          (int32Expression n)
          (int32Expression m)

    result === (n ^ m)

prop_bitwiseAnd :: Property.NamedDBProperty
prop_bitwiseAnd =
  Property.namedDBProperty "bitwiseAnd" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseAnd
          (int32Expression n)
          (int32Expression m)

    result === (n .&. m)

prop_bitwiseOr :: Property.NamedDBProperty
prop_bitwiseOr =
  Property.namedDBProperty "bitwiseOr" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseOr
          (int32Expression n)
          (int32Expression m)

    result === (n .|. m)

prop_bitwiseXor :: Property.NamedDBProperty
prop_bitwiseXor =
  Property.namedDBProperty "bitwiseXor" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseXor
          (int32Expression n)
          (int32Expression m)

    result === Bits.xor n m

prop_bitwiseShiftLeft :: Property.NamedDBProperty
prop_bitwiseShiftLeft =
  Property.namedDBProperty "bitwiseShiftLeft" $ \pool -> do
    n <- HH.forAll (Gen.integral (Range.linear 0 0xFFFFFFF))
    m <- HH.forAll (Gen.integral (Range.linear 0 64))

    result <-
      evaluateIntegerExpression pool $
        Expr.bitwiseShiftLeft
          (int32Expression n)
          (intExpression m)

    result === Bits.shiftL n (m `mod` 32)

prop_bitwiseShiftRight :: Property.NamedDBProperty
prop_bitwiseShiftRight =
  Property.namedDBProperty "bitwiseShiftRight" $ \pool -> do
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
  Conn.Pool Conn.Connection ->
  Expr.ValueExpression ->
  HH.PropertyT IO Int32
evaluateIntegerExpression pool expression = do
  let sql =
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
