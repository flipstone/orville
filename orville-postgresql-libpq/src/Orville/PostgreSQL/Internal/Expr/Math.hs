module Orville.PostgreSQL.Internal.Expr.Math
  ( plus,
    minus,
    multiply,
    divide,
    modulo,
    exponentiate,
    bitwiseAnd,
    bitwiseOr,
    bitwiseXor,
    bitwiseShiftLeft,
    bitwiseShiftRight,
  )
where

import Orville.PostgreSQL.Internal.Expr.BinaryOperator (binaryOpExpression, bitwiseAndOp, bitwiseOrOp, bitwiseShiftLeftOp, bitwiseShiftRightOp, bitwiseXorOp, divisionOp, exponentiationOp, minusOp, moduloOp, multiplicationOp, plusOp)
import Orville.PostgreSQL.Internal.Expr.ValueExpression (ValueExpression)

plus :: ValueExpression -> ValueExpression -> ValueExpression
plus =
  binaryOpExpression plusOp

minus :: ValueExpression -> ValueExpression -> ValueExpression
minus =
  binaryOpExpression minusOp

multiply :: ValueExpression -> ValueExpression -> ValueExpression
multiply =
  binaryOpExpression multiplicationOp

divide :: ValueExpression -> ValueExpression -> ValueExpression
divide =
  binaryOpExpression divisionOp

modulo :: ValueExpression -> ValueExpression -> ValueExpression
modulo =
  binaryOpExpression moduloOp

exponentiate :: ValueExpression -> ValueExpression -> ValueExpression
exponentiate =
  binaryOpExpression exponentiationOp

bitwiseAnd :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseAnd =
  binaryOpExpression bitwiseAndOp

bitwiseOr :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseOr =
  binaryOpExpression bitwiseOrOp

bitwiseXor :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseXor =
  binaryOpExpression bitwiseXorOp

bitwiseShiftLeft :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseShiftLeft =
  binaryOpExpression bitwiseShiftLeftOp

bitwiseShiftRight :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseShiftRight =
  binaryOpExpression bitwiseShiftRightOp
