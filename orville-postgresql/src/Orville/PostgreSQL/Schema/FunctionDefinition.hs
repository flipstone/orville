{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Schema.FunctionDefinition
  ( FunctionDefinition
  , setFunctionSchema
  , functionIdentifier
  , functionName
  , functionSource
  , mkTriggerFunction
  , mkCreateFunctionExpr
  ) where

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Schema.FunctionIdentifier (FunctionIdentifier, functionIdQualifiedName, setFunctionIdSchema, unqualifiedNameToFunctionId)

{- |
  Contains the definition of a PostgreSQL function for Orville to use when creating
  the function. Currently only functionality for creating trigger functions for use
  with 'Orville.PostgreSQL.Schema.TriggerDefinition' is supported. You can create a
  'FunctionDefintion' with for a trigger via 'mkTriggerFunction'.

  When a 'FunctionDefinition' is included in a schema item list for auto-migration
  the function will be created if it does not exist, or recreated if the source
  code for the function does not match what is found in the database catalog.

@since 1.1.0.0
-}
data FunctionDefinition = FunctionDefinition
  { i_functionIdentifier :: FunctionIdentifier
  , i_functionReturnType :: Expr.ReturnType
  , i_functionLanguage :: Expr.LanguageName
  , i_functionSource :: String
  }

{- |
  Sets the function's schema to the name in the given 'String', which will be
  treated as a SQL identifier. If a function has a schema name set, it will be
  included as a qualifier on the function name for all queries involving the
  function.

@since 1.0.0.0
-}
setFunctionSchema ::
  String ->
  FunctionDefinition ->
  FunctionDefinition
setFunctionSchema schemaName functionDef =
  functionDef
    { i_functionIdentifier = setFunctionIdSchema schemaName (i_functionIdentifier functionDef)
    }

{- |
  Retrieves the 'FunctionIdentifier' for this function, which is set by the
  name provided to 'mkTriggerFunction' and any calls made to
  'setFunctionSchema' thereafter.
@since 1.1.0.0
-}
functionIdentifier :: FunctionDefinition -> FunctionIdentifier
functionIdentifier =
  i_functionIdentifier

{- |
  Retrieves the 'Expr.Qualified' 'Expr.FunctionName' for the function that
  should be used to build SQL expressions involving it.

@since 1.1.0.0
-}
functionName :: FunctionDefinition -> Expr.Qualified Expr.FunctionName
functionName =
  functionIdQualifiedName . i_functionIdentifier

{- |
  Retrieves the source that was passed to 'mkTriggerFunction' when the
  'FunctionDefinition' was created.

@since 1.1.0.0
-}
functionSource :: FunctionDefinition -> String
functionSource =
  i_functionSource

{- |
  Constructs a 'FunctionDefinition' that will create a PostgreSQL function
  with a return type of @trigger@ using the specified lanugage and function
  body.
@since 1.1.0.0
-}
mkTriggerFunction ::
  String ->
  Expr.LanguageName ->
  String ->
  FunctionDefinition
mkTriggerFunction name language source =
  FunctionDefinition
    { i_functionIdentifier = unqualifiedNameToFunctionId name
    , i_functionReturnType = Expr.returnTypeTrigger
    , i_functionLanguage = language
    , i_functionSource = source
    }

{- |
  Builds a 'Expr.CreateFunctionExpr' that will create a SQL function matching
  the given 'FunctionDefinition' when it is executed.

@since 1.1.0.0
-}
mkCreateFunctionExpr ::
  Maybe Expr.OrReplace ->
  FunctionDefinition ->
  Expr.CreateFunctionExpr
mkCreateFunctionExpr maybeOrReplace functionDef =
  Expr.createFunction
    maybeOrReplace
    (functionName functionDef)
    (Expr.returns (i_functionReturnType functionDef))
    (Expr.language (i_functionLanguage functionDef))
    (Expr.asDefinition (i_functionSource functionDef))
