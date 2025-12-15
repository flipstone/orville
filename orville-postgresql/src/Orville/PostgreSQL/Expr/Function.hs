{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Copyright : Flipstone Technology Partners 2024
License   : MIT
Stability : Stable

@since 1.1.0.0
-}
module Orville.PostgreSQL.Expr.Function
  ( DropFunctionExpr
  , dropFunction
  , CreateFunctionExpr
  , createFunction
  , FunctionReturns
  , returns
  , ReturnType
  , returnTypeTrigger
  , returnTypeText
  , returnTypeUUID
  , FunctionLanguage
  , language
  , LanguageName
  , plpgsql
  , FunctionDefinition
  , asDefinition
  ) where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)

import Orville.PostgreSQL.Expr.IfExists (IfExists)
import Orville.PostgreSQL.Expr.Name (FunctionName, QualifiedOrUnqualified)
import Orville.PostgreSQL.Expr.OrReplace (OrReplace)
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import Orville.PostgreSQL.Schema.FunctionArgument (FunctionArgument)

{- | Type to represent a SQL "DROP FUNCTION" statement. E.G.

> DROP FUNCTION my_function

'DropFunctionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype DropFunctionExpr
  = DropFunctionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a SQL @DROP FUNCTION@ statement from a function name.

@since 1.1.0.0
-}
dropFunction :: Maybe IfExists -> QualifiedOrUnqualified FunctionName -> DropFunctionExpr
dropFunction maybeIfExists name =
  DropFunctionExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just (RawSql.fromString "DROP FUNCTION")
          , fmap RawSql.toRawSql maybeIfExists
          , Just (RawSql.toRawSql name)
          ]
      )

{- | Type to represent a SQL "CREATE FUNCTION" statement. E.G.

> CREATE FUNCTION my_function RETURNS trigger AS '<definition>'

'CreateFunctionExpr' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype CreateFunctionExpr
  = CreateFunctionExpr RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a SQL @CREATE FUNCTION@ statement from a function name, return type,
language and definiton.

Note: Orville does not currently support creating functions with arguments.

@since 1.1.0.0
-}
createFunction ::
  Maybe OrReplace ->
  QualifiedOrUnqualified FunctionName ->
  [FunctionArgument] ->
  FunctionReturns ->
  FunctionLanguage ->
  FunctionDefinition ->
  CreateFunctionExpr
createFunction maybeOrReplace name args functionReturns functionLanguage definition =
  CreateFunctionExpr $
    RawSql.intercalate
      RawSql.space
      ( catMaybes
          [ Just $ RawSql.fromString "CREATE"
          , fmap RawSql.toRawSql maybeOrReplace
          , Just $ RawSql.fromString "FUNCTION"
          , Just $ RawSql.toRawSql name
          , Just $ RawSql.parenthesized (RawSql.intercalate RawSql.commaSpace $ RawSql.toRawSql <$> args)
          , Just $ RawSql.toRawSql functionReturns
          , Just $ RawSql.toRawSql functionLanguage
          , Just $ RawSql.toRawSql definition
          ]
      )

{- | Type to represent the return specifier given as part of a SQL "CREATE
FUNCTION" statement. E.G. the @RETURNS trigger@ in

> CREATE FUNCTION my_function RETURNS trigger

'FunctionReturns' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FunctionReturns
  = FunctionReturns RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a @RETURNS@ specifier for a @CREATE FUNCTION@ statement with the
  given return type.

@since 1.1.0.0
-}
returns :: ReturnType -> FunctionReturns
returns returnType =
  FunctionReturns $ RawSql.fromString "RETURNS " <> RawSql.toRawSql returnType

{- | Type to represent the return type given as part of a SQL "CREATE
FUNCTION" statement. E.G. the @trigger@ in

> CREATE FUNCTION my_function RETURNS trigger

'ReturnType' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype ReturnType
  = ReturnType RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @trigger@ return type.

@since 1.1.0.0
-}
returnTypeTrigger :: ReturnType
returnTypeTrigger =
  ReturnType (RawSql.fromString "trigger")

{- | The @text@ return type.

@since 1.1.0.0.4
-}
returnTypeText :: ReturnType
returnTypeText =
  ReturnType (RawSql.fromString "text")

{- | The @uuid@ return type.

@since 1.1.0.0.4
-}
returnTypeUUID :: ReturnType
returnTypeUUID =
  ReturnType (RawSql.fromString "uuid")

{- | Type to represent the language specifier given as part of a SQL "CREATE
FUNCTION" statement. E.G. the @LANGUAGE plpgsql@ in

> CREATE FUNCTION my_function LANGUAGE plpgsql

'FunctionLanguage' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FunctionLanguage
  = FunctionLanguage RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a @LANGUAGE@ specifier for a @CREATE FUNCTION@ statement with the
  given language name as the language.

@since 1.1.0.0
-}
language :: LanguageName -> FunctionLanguage
language name =
  FunctionLanguage $ RawSql.fromString "LANGUAGE " <> RawSql.toRawSql name

{- | Type to represent the language that the function definition is given in as part
of a SQL "CREATE FUNCTION" statement. E.G. the @plpgsql@ in

> CREATE FUNCTION my_function LANGUAGE plpgsql

'LanguageName' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype LanguageName
  = LanguageName RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | The @plpgsql@ language.

@since 1.1.0.0
-}
plpgsql :: LanguageName
plpgsql =
  LanguageName (RawSql.fromString "plpgsql")

{- | Type to represent the definition body of a a SQL "CREATE FUNCTION" statement. E.G. the
@AS <definition>@ in

> CREATE FUNCTION my_function AS <definition>

'FunctionDefinition' provides a 'RawSql.SqlExpression' instance. See
'RawSql.unsafeSqlExpression' for how to construct a value with your own custom
SQL.

@since 1.1.0.0
-}
newtype FunctionDefinition
  = FunctionDefinition RawSql.RawSql
  deriving
    ( -- | @since 1.1.0.0
      RawSql.SqlExpression
    )

{- | Constructs a 'FunctionDefinition' from a 'String', which will be passed as an
  escaped string literal to PosgreSQL.

@since 1.1.0.0
-}
asDefinition :: String -> FunctionDefinition
asDefinition body =
  FunctionDefinition $
    RawSql.fromString "AS " <> RawSql.stringLiteral (BS8.pack body)
