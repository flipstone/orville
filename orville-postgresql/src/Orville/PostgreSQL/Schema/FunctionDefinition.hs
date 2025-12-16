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
  , functionAutoMigrationStep
  , mkTriggerFunction
  , mkFunction
  , mkCreateFunctionExpr
  , FunctionAutoMigrationStep (BeforeTableMigration, AfterTableMigration)
  ) where

import qualified Orville.PostgreSQL.Expr as Expr
import Orville.PostgreSQL.Schema.FunctionArgument (FunctionArgument)
import Orville.PostgreSQL.Schema.FunctionIdentifier (FunctionIdentifier, functionIdQualifiedName, setFunctionIdSchema, unqualifiedNameToFunctionId)

{- | Contains the definition of a PostgreSQL function for Orville to use when creating
  the function.
  Only 'Orville.PostgreSQL.Schema.TriggerDefinition' is currently properly supported. You can create a
  'FunctionDefintion' for a trigger via 'mkTriggerFunction' or use 'mkFunction' for (currently mostly unsupported) extra customization.

  When a 'FunctionDefinition' is included in a schema item list for auto-migration
  the function will be created if it does not exist, or recreated if the source
  code for the function does not match what is found in the database catalog.
  The 'FunctionAutoMigrationStep' determines whether the function is migrated before or after tables.
  If the function depends on tables existing it must come after table migration.
  If the tables use the function (such as calling it to populate the default value for a field) then the function
  must be migrated before the tables.

@since 1.1.0.0
-}
data FunctionDefinition = FunctionDefinition
  { i_functionIdentifier :: FunctionIdentifier
  , i_functionArguments :: [FunctionArgument]
  , i_functionReturnType :: Expr.ReturnType
  , i_functionLanguage :: Expr.LanguageName
  , i_functionSource :: String
  , i_functionAutoMigrationStep :: FunctionAutoMigrationStep
  }

{-
  The 'FunctionAutoMigrationStep' determines whether the function is migrated before or after tables.
  If the function depends on tables existing it must come after table migration.
  If the tables use the function (such as calling it to populate the default value for a field) then the function
  must be migrated before the tables.

@since 1.2.0.0
-}
data FunctionAutoMigrationStep = BeforeTableMigration | AfterTableMigration

{- | Sets the function's schema to the name in the given 'String', which will be
  treated as a SQL identifier. If a function has a schema name set, it will be
  included as a qualifier on the function name for all queries involving the
  function.

@since 1.1.0.0
-}
setFunctionSchema ::
  String ->
  FunctionDefinition ->
  FunctionDefinition
setFunctionSchema schemaName functionDef =
  functionDef
    { i_functionIdentifier = setFunctionIdSchema schemaName (i_functionIdentifier functionDef)
    }

{- | Retrieves the 'FunctionIdentifier' for this function, which is set by the
  name provided to 'mkTriggerFunction' or 'mkFunction' and any calls made to
  'setFunctionSchema' thereafter.
@since 1.1.0.0
-}
functionIdentifier :: FunctionDefinition -> FunctionIdentifier
functionIdentifier =
  i_functionIdentifier

{- | Retrieves the 'Expr.Qualified' 'Expr.FunctionName' for the function that
  should be used to build SQL expressions involving it.

@since 1.1.0.0
-}
functionName :: FunctionDefinition -> Expr.QualifiedOrUnqualified Expr.FunctionName
functionName =
  functionIdQualifiedName . i_functionIdentifier

{- | Retrieves the source that was passed in when the
  'FunctionDefinition' was created.

@since 1.1.0.0
-}
functionSource :: FunctionDefinition -> String
functionSource =
  i_functionSource

{- | Retrieves the auto migration step type passed in when the
  'FunctionDefinition' was created.

@since 1.2.0.0
-}
functionAutoMigrationStep :: FunctionDefinition -> FunctionAutoMigrationStep
functionAutoMigrationStep =
  i_functionAutoMigrationStep

{- | Constructs a 'FunctionDefinition' that will create a PostgreSQL function
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
    , i_functionArguments = []
    , i_functionReturnType = Expr.returnTypeTrigger
    , i_functionLanguage = language
    , i_functionSource = source
    , i_functionAutoMigrationStep = AfterTableMigration
    }

{- | Constructs a 'FunctionDefinition' that will create a PostgreSQL function
  using the specified lanugage, return type and function body.
@since 1.2.0.0
-}
mkFunction ::
  String ->
  [FunctionArgument] ->
  Expr.ReturnType ->
  FunctionAutoMigrationStep ->
  Expr.LanguageName ->
  String ->
  FunctionDefinition
mkFunction name args returnType autoMigrationStep language source =
  FunctionDefinition
    { i_functionIdentifier = unqualifiedNameToFunctionId name
    , i_functionArguments = args
    , i_functionReturnType = returnType
    , i_functionLanguage = language
    , i_functionSource = source
    , i_functionAutoMigrationStep = autoMigrationStep
    }

{- | Builds a 'Expr.CreateFunctionExpr' that will create a SQL function matching
  the given 'FunctionDefinition' when it is executed.

@since 1.1.0.0
-}
mkCreateFunctionExpr ::
  FunctionDefinition ->
  Maybe Expr.OrReplace ->
  Expr.CreateFunctionExpr
mkCreateFunctionExpr functionDef maybeOrReplace =
  Expr.createFunction
    maybeOrReplace
    (functionName functionDef)
    (i_functionArguments functionDef)
    (Expr.returns (i_functionReturnType functionDef))
    (Expr.language (i_functionLanguage functionDef))
    (Expr.asDefinition (i_functionSource functionDef))
