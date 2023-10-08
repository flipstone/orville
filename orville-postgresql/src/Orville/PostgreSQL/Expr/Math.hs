{- |
Copyright : Flipstone Technology Partners 2023
License   : MIT
Stability : Stable

@since 1.0.0.0
-}
module Orville.PostgreSQL.Expr.Math
  ( plus
  , minus
  , multiply
  , divide
  , modulo
  , exponentiate
  , bitwiseAnd
  , bitwiseOr
  , bitwiseXor
  , bitwiseShiftLeft
  , bitwiseShiftRight
  )
where

import Orville.PostgreSQL.Expr.BinaryOperator (binaryOpExpression, bitwiseAndOp, bitwiseOrOp, bitwiseShiftLeftOp, bitwiseShiftRightOp, bitwiseXorOp, divisionOp, exponentiationOp, minusOp, moduloOp, multiplicationOp, plusOp)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression)

{- | Apply a SQL + to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
plus :: ValueExpression -> ValueExpression -> ValueExpression
plus =
  binaryOpExpression plusOp

{- | Apply a SQL - to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
minus :: ValueExpression -> ValueExpression -> ValueExpression
minus =
  binaryOpExpression minusOp

{- | Apply a SQL * to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
multiply :: ValueExpression -> ValueExpression -> ValueExpression
multiply =
  binaryOpExpression multiplicationOp

{- | Apply a SQL / to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
divide :: ValueExpression -> ValueExpression -> ValueExpression
divide =
  binaryOpExpression divisionOp

{- | Apply a SQL % to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
modulo :: ValueExpression -> ValueExpression -> ValueExpression
modulo =
  binaryOpExpression moduloOp

{- | Apply a SQL ^ to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
exponentiate :: ValueExpression -> ValueExpression -> ValueExpression
exponentiate =
  binaryOpExpression exponentiationOp

{- | Apply a SQL & to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
bitwiseAnd :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseAnd =
  binaryOpExpression bitwiseAndOp

{- | Apply a SQL | to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
bitwiseOr :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseOr =
  binaryOpExpression bitwiseOrOp

{- | Apply a SQL # to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
bitwiseXor :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseXor =
  binaryOpExpression bitwiseXorOp

{- | Apply a SQL << to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
bitwiseShiftLeft :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseShiftLeft =
  binaryOpExpression bitwiseShiftLeftOp

{- | Apply a SQL >> to the 'ValueExpression's. It is left to the caller to ensure that the operator
 makes sense with the arguments being passed.

@since 1.0.0.0
-}
bitwiseShiftRight :: ValueExpression -> ValueExpression -> ValueExpression
bitwiseShiftRight =
  binaryOpExpression bitwiseShiftRightOp
