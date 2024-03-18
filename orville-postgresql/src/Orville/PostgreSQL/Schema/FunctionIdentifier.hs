{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Schema.FunctionIdentifier
  ( FunctionIdentifier
  , unqualifiedNameToFunctionId
  , setFunctionIdSchema
  , functionIdQualifiedName
  , functionIdUnqualifiedName
  , functionIdSchemaName
  , functionIdToString
  , functionIdUnqualifiedNameString
  , functionIdSchemaNameString
  )
where

import qualified Orville.PostgreSQL.Expr as Expr

{- |
  An identifier used by Orville to identify a particular function in a particular
  schema.

@since 1.1.0.0
-}
data FunctionIdentifier = FunctionIdentifier
  { i_functionIdName :: String
  , i_functionIdSchema :: Maybe String
  }
  deriving (Eq, Ord, Show)

{- |
  Constructs a 'FunctionIdentifier' where the function's name will not be qualified
  by a particular schema.

@since 1.1.0.0
-}
unqualifiedNameToFunctionId :: String -> FunctionIdentifier
unqualifiedNameToFunctionId name =
  FunctionIdentifier
    { i_functionIdName = name
    , i_functionIdSchema = Nothing
    }

{- |
  Sets the schema of the 'FunctionIdentifier'. Wherever applicable, references to
  the function will be qualified by the given schema name.

@since 1.1.0.0
-}
setFunctionIdSchema :: String -> FunctionIdentifier -> FunctionIdentifier
setFunctionIdSchema schema functionId =
  functionId
    { i_functionIdSchema = Just schema
    }

{- |
  Returns the 'Expr.Qualified Expr.FunctionName' that should be used to refer to
  the function in SQL queries.

@since 1.1.0.0
-}
functionIdQualifiedName :: FunctionIdentifier -> Expr.Qualified Expr.FunctionName
functionIdQualifiedName functionId =
  Expr.qualifyFunction
    (functionIdSchemaName functionId)
    (functionIdUnqualifiedName functionId)

{- |
  Returns the unqualified 'Expr.FunctionName' that should be used to refer to the
  function in SQL queries where an unqualified reference is appropriate.

@since 1.1.0.0
-}
functionIdUnqualifiedName :: FunctionIdentifier -> Expr.FunctionName
functionIdUnqualifiedName =
  Expr.functionName . i_functionIdName

{- |
  Returns the 'Expr.SchemaName' (if any) that should be used to qualify
  references to the function in SQL queries.

@since 1.1.0.0
-}
functionIdSchemaName :: FunctionIdentifier -> Maybe Expr.SchemaName
functionIdSchemaName =
  fmap Expr.schemaName . i_functionIdSchema

{- |
  Retrieves the unqualified name of the function as a 'String'.

@since 1.1.0.0
-}
functionIdUnqualifiedNameString :: FunctionIdentifier -> String
functionIdUnqualifiedNameString =
  i_functionIdName

{- |
  Retrieves the schema name of the function as a 'String'.

@since 1.1.0.0
-}
functionIdSchemaNameString :: FunctionIdentifier -> Maybe String
functionIdSchemaNameString =
  i_functionIdSchema

{- |
  Converts a 'FunctionIdentifier' to a 'String' for descriptive purposes. The
  name will be qualified if a schema name has been set for the identifier.

  Note: You should not use this function for building SQL expressions. Use
  'functionIdQualifiedName' instead for that.

@since 1.1.0.0
-}
functionIdToString :: FunctionIdentifier -> String
functionIdToString functionId =
  case i_functionIdSchema functionId of
    Nothing ->
      i_functionIdName functionId
    Just schema ->
      schema <> "." <> i_functionIdName functionId
