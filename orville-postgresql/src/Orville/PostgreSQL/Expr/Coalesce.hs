{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Coalesce
  ( coalesce
  , coalesceFunction
  )
where

import qualified Data.List.NonEmpty as NE

import Orville.PostgreSQL.Expr.Name (FunctionName, functionName)
import Orville.PostgreSQL.Expr.ValueExpression (ValueExpression, functionCall)

{- | The SQL @coalesce@ function.

@since 1.1.0.0
-}
coalesceFunction :: FunctionName
coalesceFunction = functionName "coalesce"

{- | Use a non-empty list of 'ValueExpression' as the arguments to the @coalesce@ function.

@since 1.1.0.0
-}
coalesce :: NE.NonEmpty ValueExpression -> ValueExpression
coalesce values =
  functionCall coalesceFunction (NE.toList values)
